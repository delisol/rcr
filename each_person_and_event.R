# each_person_and_event.R
# pivots to one line per person
# use for indexing info, not for payments

# packages needed
library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(googlesheets4)
library(googledrive)
library(rlang)

# end packages needed

# user-defined functions

# end user-defined functions

# start with clean auth plate
gs4_deauth()
gs4_user()

# get auth
drive_auth(email = 'dsolet@gmail.com')
gs4_auth(token = drive_token())

dynName <- 'form_responses'
df <-
  read_sheet(
    'https://docs.google.com/spreadsheets/d/1cDcCu0YGcrDsRqonGTfnGc2l2rcdYiMgu3ipbrUJck8/edit?usp=sharing',
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
  filter(last_name != 'McTesterface') %>%
  filter(last_name != 'MCTESTERFACE') %>%
  mutate(sce = dynName) %>%
  mutate(acct = sprintf('%s, %s', fee_str, event)) %>%
  relocate(sce, .after = last_col())
dynName <- ''


orig_names <- read_csv(
  'orig_names.csv',
  col_types = cols(.default = col_character())
)[[1]]
new_names <- read_csv(
  'tr_names.csv',
  col_types = cols(.default = col_character())
)[[1]]
lookup <- setNames(orig_names, new_names)

t <- df %>% rename(all_of(lookup))

each_person <- t |>
  rename(
    RegPlusGuests = how_many_people_are_you_registering_including_yourself
  ) %>%
  mutate(registration_id = row_number()) |>
  pivot_longer(
    cols = matches("^(first|last|email|voice_part|instrument)_[1-4]$"),
    names_to = c(".value", "person_number"),
    names_pattern = "^(first|last|email|voice_part|instrument)_([1-4])$",
    names_transform = list(person_number = as.integer)
  ) |>
  filter(person_number <= RegPlusGuests) %>%
  select(
    event,
    first,
    last,
    email,
    fee_str,
    fee_num,
    submission_date,
    today,
    scores,
    packets,
    registration_id,
    person_number
  )

by_event <- split(each_person, each_person$event)

toc <- data.frame(
  index = seq_along(by_event),
  event = names(by_event)
)

iberian <- by_event[[1]] %>%
  arrange(registration_id, person_number)
tabyl(iberian$fee_str)

cusp <- by_event[[2]] %>%
  arrange(registration_id, person_number)
tabyl(cusp$fee_str)

courtly <- by_event[[3]] %>%
  arrange(registration_id, person_number)
tabyl(courtly$fee_str)

may <- by_event[[4]] %>%
  arrange(registration_id, person_number)
tabyl(may$fee_str)
