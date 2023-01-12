# setup -------------------------------------------------------------------
library(philsfmisc)
# library(data.table)
library(tidyverse)
library(readxl)
library(haven)
# library(foreign)
# library(lubridate)
# library(naniar)
library(labelled)

# data loading ------------------------------------------------------------
set.seed(42)
# data.raw <- tibble(id=gl(2, 10), exposure = gl(2, 10), outcome = rnorm(20))
data.raw <- read_sav("dataset/Form1_20221017.sav") %>%
  # join second patient data table
  left_join(
    read_sav("dataset/Form2_20221017.sav"), by = c("Mod1id", "EntryDate"),
  )

data.raw %>%
  left_join(
    read_excel("dataset/DCI.xlsx") %>%
      mutate(Zipcode = formatC(Zipcode, width = 5, flag = "0")), # fix zipcodes with leading zeroes
    by = c("ZipDis" = "Zipcode")
  )
Nvar_orig <- data.raw %>% ncol
Nobs_orig <- data.raw %>% nrow

# data cleaning -----------------------------------------------------------

data.raw <- data.raw %>%
  select(
    everything(),
  ) %>%
  rename(
  ) %>%
  mutate(
  ) %>%
  filter(
  )

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  mutate(
    id = factor(id), # or as.character
  )
