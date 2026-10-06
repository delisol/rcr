library(tidyverse)
library(janitor)
library(here)
library(lubridate)

setwd('C:\\Users\\dsole\\OneDrive\\Personal Vault\\data\\rcr')

# last Stripe deposit in gnucash
date_last_gnu <- '2026-09-02'

orders <-
  read_csv(
    file = 'orders.csv',
    col_types = cols(.default = 'c')
  ) %>%
  clean_names()

donations <-
  read_csv(
    file = 'donations.csv',
    col_types = cols(.default = 'c')
  ) %>%
  clean_names()

orders_donations <-
  bind_rows(orders, donations) %>%
  # fix one-time anomalies caused by payment structure
  filter(!is.na(email)) %>%
  filter(total != '0.00') %>%
  select(
    paid_at,
    email,
    total,
    lineitem_name,
    billing_name,
    billing_address1,
    billing_address2,
    billing_city,
    billing_zip,
    billing_province,
    billing_country,
    billing_phone,
    payment_reference
  )

# payouts

balance_history <-
  read_csv(
    file = 'balance_history.csv',
    col_types = cols(.default = 'c')
  ) %>%
  clean_names() %>%
  # filtering to last date squarespace registration was used for workshops
  filter(as.Date(substr(transfer_date_utc, 1, 10)) >= as.Date('2026-09-02')) %>%
  select(
    type,
    source,
    amount,
    fee,
    net,
    created_utc,
    description,
    transfer,
    transfer_date_utc
  )

fee <- balance_history %>%
  filter(fee != '0.00') %>%
  filter(!is.na(transfer)) %>%
  group_by(transfer, transfer_date_utc) %>%
  summarise(amount = sum(as.numeric(fee))) %>%
  mutate(amount = as.character(amount), type = 'fee', description = 'cc fee')

contributions <-
  balance_history %>%
  filter(type == 'contribution') %>%
  group_by(transfer, transfer_date_utc) %>%
  summarise(amount = sum(as.numeric(amount))) %>%
  mutate(amount = as.character(amount), type = 'contribution')

combined <-
  balance_history %>%
  filter(
    type != 'payout' &
      type != 'contribution'
  ) %>%
  bind_rows(., contributions, fee) %>%
  arrange(transfer) %>%
  left_join(., orders_donations, by = c('source' = 'payment_reference')) %>%
  filter(!is.na(paid_at)) %>%
  bind_rows(
    balance_history %>%
      filter(type == 'payout'),
    .
  ) %>%
  arrange(transfer) %>%
  filter(
    transfer == lag(transfer) |
      transfer == lead(transfer)
  ) %>%
  bind_rows(., fee) %>%
  arrange(transfer) %>%
  filter(
    transfer == lag(transfer) |
      transfer == lead(transfer)
  )


t <- combined %>%
  filter(
    !is.na(lineitem_name) |
      description == 'STRIPE PAYOUT'
  )

# set wd locally out of vault
setwd(here())
for_gnucash <-
  combined %>%
  # appropriate math signs for amounts
  mutate(amount = as.numeric(amount)) %>%
  mutate(
    dep_amt = ifelse(
      type == 'charge' |
        type == 'payment' |
        type == 'refund' |
        type == 'payout' |
        type == 'stripe_fee' |
        type == 'contribution',
      -amount,
      amount
    )
  ) %>%
  mutate(
    num = ifelse(
      type == 'payout',
      'DEP',
      NA
    )
  ) %>%
  mutate(
    date_of_deposit = ifelse(
      type == 'payout',
      as.character.Date(transfer_date_utc),
      NA
    )
  ) %>%
  mutate(desc = ifelse(description == 'STRIPE PAYOUT', description, NA)) %>%
  mutate(
    account = ifelse(
      !is.na(lineitem_name),
      sprintf('%s , %s', type, lineitem_name),
      type
    )
  ) %>%
  mutate(
    memo = ifelse(
      type == 'charge',
      sprintf(
        'Refno: %s , on %s',
        source,
        as.character.Date(created_utc)
      ),
      NA
    )
  ) %>%
  # filter to post latest gnucash deposit
  filter(
    transfer_date_utc >= date_last_gnu
  ) %>%
  # filter to payments already paid out
  filter(!is.na(transfer)) %>%
  # refunds
  mutate(
    memo = ifelse(
      str_detect(account, 'refund') & is.na(memo),
      account,
      memo
    )
  ) %>%
  select(num, date_of_deposit, desc, account, dep_amt, memo)

write.csv(for_gnucash, 'for_gnucash_sqsp.csv', na = '')

# erase downloads from Stripe
setwd('C:\\Users\\dsole\\OneDrive\\Personal Vault\\data\\rcr')

file.remove('balance_history.csv')
file.remove('orders.csv')
file.remove('donations.csv')

# reset wd
setwd(here())
