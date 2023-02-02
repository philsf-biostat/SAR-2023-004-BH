# setup -------------------------------------------------------------------
# library(moderndive)
library(broom)
# library(lmerTest)
# library(broom.mixed)
# library(simputation)
# library(mice)
library(survival)

md <- analytical %>%
  select(-id, -PriorSeiz, -Mar,) %>%
  drop_na()

# raw estimate ------------------------------------------------------------

mod.crude <- coxph(Surv(Time/dyears(1), outcome) ~ exposure, md)

# adjusted ----------------------------------------------------------------

mod.full <- coxph(Surv(Time/dyears(1), outcome) ~ exposure + ., md)

# tab_inf <- tbl_merge(
#   tbls = list(
#     mod.crude %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p(), # crude HR
#     mod.full %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p() # aHR
#     ),
#   tab_spanner = c("Crude estimate", "Adjusted estimate")
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
