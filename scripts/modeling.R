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
