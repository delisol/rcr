# impTransRCR26.2.R
# Rewritten to match on submission_id
# FOR JOTFORM ENTRIES

# packages needed
library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(googlesheets4)
library(googledrive)
library(rlang)
library(readxl)

# end packages needed

# user-defined functions

# end user-defined functions

# enter the last Stripe deposit in gnucash
dteLastGnu <- '2026-08-12'

# start with clean auth plate
gs4_deauth()
gs4_user()

# get auth
drive_auth(email = 'dsolet@gmail.com')
gs4_auth(token = drive_token())


# read in all transactions (balance_history.csv)
all_tx <-
  read_csv(
    'C:\\Users\\dsole\\OneDrive\\Personal Vault\\data\\rcr\\balance_history.csv',
    col_types = cols(`submissionID (metadata)` = col_character())
  ) %>%
  clean_names() %>%
  filter(type != 'adjustment') %>%
  select(
    type,
    amount,
    fee,
    created_utc,
    transfer_date_utc,
    transfer,
    source,
    submission_id_metadata
  )


# limit deposits (transfers) to after date of last gnucash deposit
# transfer identifies a payout and pulls all transactions in a particular payout
payouts_plus <-
  all_tx %>%
  filter(type == 'payout') %>%
  filter(lubridate::ymd(transfer_date_utc) >= dteLastGnu) %>%
  select(transfer, type) %>%
  left_join(., all_tx, by = 'transfer') %>%
  mutate(type = type.y) %>%
  select(-type.x, -type.y)

#######################################################################
# workshops downloaded from jotform
# temporarily in .xls format on onedrive

dynName <- 'regis_exc'
df <-
  read_excel(
    'C:/Users/dsole/OneDrive/Documents/RCR/RCR Workshop Registration.xlsx',
    sheet = 'Form responses'
  ) %>%
  clean_names() %>%
  rename_with(~ str_replace(., '^name_', ''), starts_with('name_')) %>%
  #  mutate(name = sprintf('%s %s' , first_name , last_name)) %>%
  rename(
    event = which_event_are_you_registering_for,
    scores = how_would_you_like_your_scores,
    packets = how_many_printed_packets_20_00_each,
    reduced = reduced_fee_registration_20_30,
    forward = pay_it_forward_50_thank_you
  ) %>%
  mutate(fee_str = coalesce(fee_a, fee_b, fee_c)) %>%
  mutate(
    fee_num = recode(
      fee_str,
      'Standard Fee ($40)' = 40,
      'RCR Singer ($20)' = 20,
      'Reduced Fee Registration ($20-$30)' = as.numeric(reduced),
      'Pay It Forward ($50 or more, thank you!)' = forward,
      .default = as.numeric(0)
    )
  ) %>%
  mutate(fee_score = coalesce(20 * packets, 0)) %>%
  select(
    first_name,
    last_name,
    email,
    event,
    fee_total_hidden,
    submission_id,
    submission_date,
    fee_str,
    fee_num,
    fee_score
  ) %>%
  mutate(sce = dynName) %>%
  mutate(acct = event) %>%
  relocate(sce, .after = last_col())
assign(dynName, df)
dynName <- ''

# create list of files to concatenate >>>> jotFall
# fileNames <- ls(pattern = '_202|25_|26_|27_')
# fileList <- mget(fileNames)

# add workshop datasets together with membersignup
# jotFall <-
#   bind_rows(fileList) %>%
#   mutate(membership_type = str_replace_all(membership_type, 'Free', '$0')) %>%
#   mutate(
#     join_reg = parse_number(membership_type),
#     donNum = parse_number(additional_donation),
#     sub_dt_tz = lubridate::ymd_hms(submission_date, tz = Sys.timezone())
#   ) %>%
#   mutate(
#     created_utc = lubridate::floor_date(
#       with_tz(sub_dt_tz, 'GMT'),
#       unit = 'minute '
#     )
#   ) %>%
#   mutate(acct = sce) %>%
#   select(
#     submission_date,
#     created_utc,
#     first_name,
#     last_name,
#     email,
#     payment_intent_id,
#     customer_id,
#     acct,
#     join_reg,
#     donNum,
#     submission_id,
#     sce
#   )

payouts_plus_regis <- left_join(
  payouts_plus %>%
    rename(submission_id = submission_id_metadata),
  regis_exc,
  by = 'submission_id'
) %>%
  mutate(amount = ifelse(type != 'payout', fee_num, amount))

forGnucash <-
  payouts_plus_regis %>%
  rename(regis_fee = amount, fee_cc = fee, score_fee = fee_score) %>%
  ########### create bank account ###########
  mutate(acct = case_when(type == 'payout' ~ '1Bank', TRUE ~ acct)) %>%
  mutate(regis_fee = -regis_fee, score_fee = -score_fee) %>%
  pivot_longer(
    cols = c(regis_fee, score_fee, fee_cc),
    names_to = 'charge_type',
    values_to = 'amt_acct'
  ) %>%
  ########### clean amt_acct ###########
  mutate(amt_acct = na_if(amt_acct, 0)) %>%
  filter(!is.na(amt_acct)) %>%
  ########### create final deposit amount ###########
  # mutate(
  #   amt_dep = ifelse(
  #     charge_type != 'fee',
  #     -amt_acct,
  #     amt_acct
  #   )
  # ) %>%
  ########### create required variables ###########
  mutate(
    num = ifelse(
      acct == '1Bank',
      'DEP',
      NA
    ),
    date_of_dep = ifelse(
      acct == '1Bank',
      format(as.Date(transfer_date_utc), format = "%m/%d/%Y"),
      NA
    ),
    descriptx = ifelse(
      acct == '1Bank',
      'Stripe deposit',
      NA
    )
  ) %>%
  mutate(
    acct = ifelse(acct != '1Bank', sprintf('%s, %s', acct, charge_type), acct)
  ) %>%
  mutate(
    memo = ifelse(
      str_detect(acct, 'Bank', negate = TRUE),
      sprintf(
        '%s on %s, RefNo %s',
        sce,
        str_sub(submission_date, 1, 10),
        submission_id
      ),
      ''
    )
  ) %>%
  ########### final content ###########
  select(
    num,
    date_of_dep,
    descriptx,
    acct,
    amt_acct,
    memo
  )

# save to csv for gnucash
write.csv(forGnucash, 'forGnucash.csv', na = '')

# clean up personal folder
setwd('C:/Users/dsole/OneDrive/Personal Vault/data/rcr')
file.remove('balance_history.csv')
file.remove('orders.csv')
file.remove('donations.csv')
setwd(here())

getwd()
### examples from previous years
####### additions and corrections #################
# adding Dick Yates donation
{
  if (dteLastGnu == '2024-11-29') {
    add_row(
      .,
      acct = 'Other donations, 2024-25',
      amt_dep = -2000,
      memo = 'Dick Yates donation using website link',
      .after = which(.$date_of_dep == '12/11/2024')
    )
  } else {
    .
  }
} %>%
  mutate(
    acct = ifelse(acct == 'NA, fee', 'Other donations , 2024-25 , fee', acct),
    memo = ifelse(
      memo == 'NA NA, NA on NA',
      'Dick Yates, donation using website link, 12/10/2024',
      memo
    )
  ) %>%
  # Abbie Crane refund
  {
    if (dteLastGnu == '2024-11-29') {
      add_row(
        .,
        acct = 'Workshop refunds, 2024-25',
        amt_dep = 20,
        memo = 'Abbie Crane refund for 12/7/24 workshop after renewing membership',
        .after = which(.$date_of_dep == '12/09/2024')
      )
    } else {
      .
    }
  } %>%
  # Karen Bartlett refund for E. Reed workshop withdrawal
  {
    if (dteLastGnu == '2025-03-10') {
      add_row(
        .,
        acct = 'Workshop refunds, 2024-25',
        amt_dep = 55,
        memo = 'Karen Bartlett refund for 3/10/2025 workshop after renewing membership',
        .after = which(.$date_of_dep == '05/05/2025')
      )
    } else {
      .
    }
  }


############# NOTE THESE IF STATEMENTS FOR ADDING REFUNDS IF NEEDED FOR REFERENCE
# if statement for adding refunds
{
  if (dteLastGnu == '2023-10-13') {
    add_row(
      .,
      acct = 'Workshop refunds',
      amt_to_acct = 50,
      memo = 'Jeffrey Allen withdrew from 2023-10-07 workshop',
      .after = which(.$dte == '2023-10-13')
    )
  } else {
    .
  }
} %>%
  {
    if (dteLastGnu == '2023-12-12') {
      add_row(
        .,
        acct = 'Workshop refunds',
        amt_to_acct = 50,
        memo = 'Karen Barlett withdrew from 2023-12-16 workshop',
        .after = which(.$dte == '2023-12-21')
      )
    } else {
      .
    }
  } %>%
  {
    if (dteLastGnu == '2024-01-10') {
      add_row(
        .,
        acct = 'dues refunds',
        amt_to_acct = 30,
        memo = 'Stephen Berman dues duplicate payment',
        .after = which(.$dte == '2024-01-31')
      )
    } else {
      .
    }
  } %>%
  {
    if (dteLastGnu == '2024-01-31') {
      add_row(
        .,
        acct = 'registration refunds',
        amt_to_acct = 50,
        memo = 'Kris Hoveler duplicate payment for 3-23-24 workshop',
        .after = which(.$dte == '2024-03-18')
      )
    } else {
      .
    }
  }

# encrypt_file('forGnucash.csv' ,
#              crypt_file_name = 'forGnucash_2022_10.encryptr.bin' ,
#              public_key_path = 'D:/Keys/id_rsa1')
# file.remove('forGnucash.csv')
#
# encrypt_file('balance_history.csv' ,
#              crypt_file_name = 'balance_history.encryptr.bin' ,
#              public_key_path = 'D:/Keys/id_rsa1')
file.remove(
  'C://Users//dsole//OneDrive//Personal Vault//data//cascadia//balance_history.csv'
)
#
#
# # NOTE: These remove unencrypted files if run
# # This has to be rewritten, because of the date part of the filename
# encryR(x = quote('unified_payments.csv') ,
#        y = quote('2022_10'))
#
# encryR(x = quote('olga.RData') ,
#        y = quote(''))
