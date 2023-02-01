# setup -------------------------------------------------------------------
library(philsfmisc)
# library(data.table)
library(tidyverse)
library(readxl)
# library(haven)
# library(foreign)
# library(lubridate)
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

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  mutate(
    id = as.character(id), # or as.factor
  )

# labels ------------------------------------------------------------------

data.raw <- data.raw %>%
  set_variable_labels(
    # exposure = "Study exposure",
    # outcome = "Study outcome",
  )

# analytical dataset ------------------------------------------------------

analytical <- data.raw %>%
  # select analytic variables
  select(
    id,
    SES,
    Injury,
    RehabDis,
    Birth,
    SexF,
    Race,
    Mar,
    ResDis,
    ZipDis,
    PriorSeiz,
    SCI,
    Cause,
    AcutePay1,
    RehabPay1,
    AGE,
    PROBLEMUse,
    DAYStoREHABdc,
    DRSd,
    EDUCATION,
    EMPLOYMENT,
    FIMTOTD,
    PTADays,
    RURALdc,
    FollowUpPeriod,
    IntStatus,
    DeathF,
    Followup,
  )

Nvar_final <- analytical %>% ncol
Nobs_final <- analytical %>% nrow

# mockup of analytical dataset for SAP and public SAR
analytical_mockup <- tibble( id = c( "1", "2", "3", "...", "N") ) %>%
# analytical_mockup <- tibble( id = c( "1", "2", "3", "...", as.character(Nobs_final) ) ) %>%
  left_join(analytical %>% head(0), by = "id") %>%
  mutate_all(as.character) %>%
  replace(is.na(.), "")
