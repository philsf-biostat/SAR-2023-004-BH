# setup -------------------------------------------------------------------
library(philsfmisc)
# library(data.table)
library(tidyverse)
# library(readxl)
# library(haven)
# library(foreign)
library(lubridate)
# library(naniar)
library(labelled)

# data loading ------------------------------------------------------------
set.seed(42)
load(file = "dataset/brennan_data.rds")

# data cleaning -----------------------------------------------------------

data.raw <- data.raw %>%
  select(
    everything(),
  ) %>%
  rename(
    id = Mod1id,
    SES = DCIDistressScore,
  ) %>%
  mutate(
  ) %>%
  filter(
  )

# rename selecting vars
demographics <- str_replace(demographics, "Mod1id", "id")
demographics <- str_replace(demographics, "DCIDistressScore", "SES")
clinical <- str_replace(clinical, "Mod1id", "id")
clinical <- str_replace(clinical, "DCIDistressScore", "SES")

# inclusion criteria: up to 10yr of follow up
data.raw <- data.raw %>%
  filter(
    FollowUpPeriod <= 10,
  )

# exclusion criteria: redundant participant observations: pick last date of follow up
data.raw <- data.raw %>%
  group_by(id) %>%
  filter(
    FollowUpPeriod == max(FollowUpPeriod, na.rm = TRUE),
  ) %>%
  ungroup()

# inclusion criteria: 10yr follow up + unique IDs
Nobs_incl_id <- data.raw %>% nrow()

# inclusion criteria: study period
data.raw <- data.raw %>%
  filter(
    between(RehabDis, as.Date("2010-01-01"), as.Date("2018-12-31")), # discharge date
    # Followup <= as.Date("2019-12-31"), # follow up date
  )

# inclusion criteria: study period
Nobs_incl_per <- data.raw %>% nrow()

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  mutate(
    id = as.character(id), # or as.factor
    # create new Date with either DeathF OR Followup - prioritize Deaths over Followup when both are present
    Date = if_else(is.na(DeathF), Followup, DeathF),
    # status at followup Date
    Status = as.numeric(!is.na(DeathF)), # 0=alive, 1=dead
    # time to event (in days)
    Time = as.duration(interval(RehabDis, Date)),
    # age at time of injury
    AGE = if_else(is.na(AGE), floor(as.duration(interval(Birth, Injury))/dyears(1)), AGE),
  )

# labels ------------------------------------------------------------------

data.raw <- data.raw %>%
  set_variable_labels(
    # exposure = "Study exposure",
    # outcome = "Study outcome",
    AGE = "Age at injury",
    # Time = "Time of follow up",
    # Date = "Date of last follow up",
    # Status = "Status at last follow up",
  )

# analytical dataset ------------------------------------------------------

analytical <- data.raw %>%
  # select analytic variables
  select(
    id,
    SES,
    Date,
    Status,
    Time,
    everything(),
    -starts_with("Zip"),
    -starts_with("DCI"),
    -where(is.Date),
    -IntStatus,
    -FollowUpPeriod,
  )

Nvar_final <- analytical %>% ncol
Nobs_final <- analytical %>% nrow

# mockup of analytical dataset for SAP and public SAR
analytical_mockup <- tibble( id = c( "1", "2", "3", "...", "N") ) %>%
# analytical_mockup <- tibble( id = c( "1", "2", "3", "...", as.character(Nobs_final) ) ) %>%
  left_join(analytical %>% head(0), by = "id") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")
