# setup -------------------------------------------------------------------
library(philsfmisc)
# library(data.table)
library(tidyverse)
library(readxl)
library(haven)
# library(foreign)

# data joining ------------------------------------------------------------

set.seed(42)

# read first patient data table
data.raw <- read_sav("dataset/Form1_20221017.sav")

# join second patient data table
data.raw <- data.raw %>%
  left_join(
    read_sav("dataset/Form2_20221017.sav"),
    by = "Mod1id",
    suffix = c("_Form1", "_Form2"),
  )

# join Zipcodes table
data.raw <- data.raw %>%
  left_join(
    read_excel("dataset/DCI.xlsx") %>%
      mutate(Zipcode = formatC(Zipcode, width = 5, flag = "0")), # fix zipcodes with leading zeroes
    by = c("ZipDis" = "Zipcode")
  )

# data cleaning -----------------------------------------------------------

data.raw <- data.raw %>%
  select(
    everything(),
  ) %>%
  rename(
  ) %>%
  mutate(
    # convert haven_labelled to factor (missing value codes are used automatically)
    across(where(is.labelled), as_factor),
  ) %>%
  filter(
  )

# data wrangling ----------------------------------------------------------

data.raw <- data.raw %>%
  mutate(
  )

# data saving -------------------------------------------------------------

write_rds(data.raw, file = "dataset/brennan_data.rds")
