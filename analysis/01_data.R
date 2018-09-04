## Mikael Poul Johannesson
## 2017

## Start Matter ------------------------------------------------------

library(here)
library(haven)
library(tidyverse)

## Load Data ---------------------------------------------------------

## The Norwegian Citizen Panel Wave 7
## Md5sum: a91e1eaa974e35a589628c73e5e49d2d
## tools::md5sum(here("raw", "Norwegian citizen panel - wave 7 EN.sav"))
w7_raw <- read_sav(
  here("raw", "Norwegian citizen panel - wave 7 EN.sav")
)

## Treatments
## Md5sum: 965f6c18a201d44cc4c7e6b3fd74326f
## tools::md5sum(here("raw", "wiggle_treatments.csv"))
data_treats <- read.csv(
  here("raw", "wiggle_treatments.csv"),
  stringsAsFactors = FALSE
)

## Prep experiment ---------------------------------------------------

wiggle_01 <-
  w7_raw %>%
  mutate(
    rsp_id = as.numeric(responseid),
    rsp_age = as.numeric(r7P5_1),
    rsp_age_cat = case_when(
      r7P5_2 == 1 ~ "18-29 yrs",
      r7P5_2 == 2 ~ "30-59 yrs",
      r7P5_2 == 3 ~ "60 yrs and above"),
    rsp_gender = case_when(
      r7P1 == 1 ~ "Male",
      r7P1 == 2 ~ "Female"),
    rsp_edu = case_when(
      r7P4_1 == 1 ~ "Lower or intermediate",
      r7P4_1 == 2 ~ "Lower or intermediate",
      r7P4_1 == 3 ~ "Higher"),
    rsp_party = case_when(
      r7k204 == 1 ~ "Christian Democracts (C)",
      r7k204 == 2 ~ "Conservative Party (CR)",
      r7k204 == 3 ~ "Progress Party (FR)",
      r7k204 == 4 ~ "Liberal Party (C)",
      r7k204 == 5 ~ "Socialist Left (L)",
      r7k204 == 6 ~ "Agrarian Party (C)",
      r7k204 == 7 ~ "Green Party (C)",
      r7k204 == 8 ~ "Labour Party (CL)",
      r7k204 == 9 ~ "Red Party (FL)")
  )

## The NCP only have the `treat_id` number of which treatment
## combination, i.e., sentence, the respondents were assigned. The
## treatment values embedded in the assigned sentences is in
## `data_treatments`.
wiggle_02 <-
  wiggle_01 %>%
  mutate(
    rsp_eu = case_when(
      r7padpilot1 == 1 ~ "For EU Membership",
      r7padpilot1 == 2 ~ "Against EU Membership",
      r7padpilot1 == 3 ~ "Don't know"),
    treat_id = as.numeric(as.character(r7padpilot2_ran)),
    post = case_when(
      r7padpilot2 == 1 ~ 1,
      r7padpilot2 == 2 ~ 0)
  ) %>%
  filter(!is.na(post)) %>%
  left_join(data_treats, by = "treat_id")

wiggle_03 <-
  wiggle_02 %>%
  mutate(
    treat_outcome = case_when(
      treat_outcome_raw == "den vinnende siden" ~ "Not shown",
      treat_outcome_raw == "ja-siden"           ~ "For EU Membership won",
      treat_outcome_raw == "nei-siden"          ~ "Against EU Membership won"),
    treat_turnout = case_when(
      treat_turnout_raw == "blank" ~ "Not shown",
      treat_turnout_raw != "blank" ~ as.character(treat_turnout_raw)),
    treat_majority = case_when(
      treat_majority_raw == "blank" ~ "Not shown",
      treat_majority_raw != "blank" ~ as.character(treat_majority_raw)),
    treat_outcome_match = case_when(
      treat_outcome_raw == "den vinnende siden"                  ~ "Not shown",
      grepl("For", rsp_eu) & grepl("ja", treat_outcome_raw)      ~ "Favourable outcome",
      grepl("Against", rsp_eu) & grepl("nei", treat_outcome_raw) ~ "Favourable outcome",
      grepl("For", rsp_eu) & grepl("nei", treat_outcome_raw)     ~ "Unfavourable outcome",
      grepl("Against", rsp_eu) & grepl("ja", treat_outcome_raw)  ~ "Unfavourable outcome"),
    treat_outcome_shown = case_when(
      treat_outcome_raw == "den vinnende siden" ~ "Shown\noutcome",
      treat_outcome_raw != "den vinnende siden" ~ "Not shown"),
    treat_turnout_shown = case_when(
      treat_turnout_raw == "blank" ~ "Not shown",
      treat_turnout_raw != "blank" ~ "Shown\nsize of turnout"),
    treat_majority_shown = case_when(
      treat_majority_raw == "blank" ~ "Not shown",
      treat_majority_raw != "blank" ~ "Shown\nsize of majority")
  ) %>%
  select(matches("rsp_"), matches("treat_"), post)

wiggle <- wiggle_03

## Write Data --------------------------------------------------------

write.csv(
  wiggle,
  file = here("data", "wiggle.csv"),
  row.names = FALSE
)
