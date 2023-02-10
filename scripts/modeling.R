# setup -------------------------------------------------------------------
# library(moderndive)
library(broom)
# library(lmerTest)
# library(broom.mixed)
# library(simputation)
# library(mice)
library(survival)

# model data
md <- analytical %>%
  select(-id, -PriorSeiz, -Mar,) %>%
  drop_na()

# raw estimate ------------------------------------------------------------

mod.crude <- coxph(Surv(Time/dyears(1), outcome) ~ exposure, md)

# adjusted ----------------------------------------------------------------

mod.full <- coxph(Surv(Time/dyears(1), outcome) ~ exposure + ., md)
mod.late <- coxph(Surv(Time/dyears(1), outcome) ~ exposure + ., filter(md, Time > dyears(1)))

# tab_inf <- tbl_merge(
#   tbls = list(
#     mod.crude %>% tbl_regression(exp = TRUE, include = exposure) %>% bold_labels() %>% bold_p(), # crude HR
#     mod.full %>% tbl_regression(exp = TRUE, include = exposure) %>% bold_labels() %>% bold_p(), # aHR
#     mod.late %>% tbl_regression(exp = TRUE, include = exposure) %>% bold_labels() %>% bold_p() # Late deaths
#     ),
#   tab_spanner = c("Crude estimate", "Adjusted estimate", "Late deaths")
#   )
# 
# write_rds(tab_inf, "dataset/tab_inf.rds")

tab_inf <- read_rds("dataset/tab_inf.rds")

# tab_desc %>%
#   as_gt() %>%
#   gtsave("table1.png")
# tab_inf %>%
#   as_gt() %>%
#   gtsave("table2.png")

# tab_app <- tbl_merge(
#   tbls = list(
#     mod.crude %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p(), # crude HR
#     mod.full %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p(), # aHR
#     mod.late %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p() # Late deaths
#   ),
#   tab_spanner = c("Crude estimate", "Adjusted estimate", "Late deaths")
# )
# 
# write_rds(tab_app, "dataset/tab_app.rds")

tab_app <- read_rds("dataset/tab_app.rds")

# predictions & curves ----------------------------------------------------

newdat <- expand.grid(
  exposure = levels(md$exposure),
  SexF = levels(md$SexF),
  # SexF = "Male",
  Race = "White",
  AGE = 31,
  PROBLEMUse = "No",
  EDUCATION = "Greater Than High School",
  EMPLOYMENT = "Employed",
  RURALdc = "Urban",
  SCI = "No",
  Cause = "Vehicular",
  RehabPay1 = "Private Insurance",
  ResDis = "Private Residence",
  DAYStoREHABdc = 42,
  FIMMOTD = 53,
  FIMCOGD = 20
  )
rownames(newdat) <- letters[1:10]
