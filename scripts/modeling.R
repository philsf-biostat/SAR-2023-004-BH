# setup -------------------------------------------------------------------
library(broom)
library(survival)

# model data
md <- analytical %>%
  select(-id, -PriorSeiz, -Mar,) %>%
  drop_na()
Nobs_model <- md %>% nrow()

# raw estimate ------------------------------------------------------------

mod.crude <- coxph(Surv(Time, outcome) ~ exposure, md)

# adjusted ----------------------------------------------------------------

mod.full <- coxph(Surv(Time, outcome) ~ exposure + ., md)

# Schoenfeld residuals
# (cox.zph(mod.full))$table %>% as.data.frame() %>% arrange(p)

# remove vars after Schoenfeld test
mod.final <- update(mod.full, . ~ .
                    # -FIMMOTD
                    -FIMCOGD
                    # -DAYStoREHABdc
                    -Cause
                    )

# # add interaction terms to the model
# mod.final <- update(mod.final, . ~ . + exposure*(RehabPay1 + RURALdc))

# late deaths (over 1 year)
mod.late <- update(mod.final, data = filter(md, Time > 1))

# tab_inf <- tbl_merge(
#   tbls = list(
#     mod.crude %>% tbl_regression(exp = TRUE, include = exposure) %>% bold_labels() %>% bold_p(), # crude HR
#     mod.final %>% tbl_regression(exp = TRUE, include = exposure) %>% bold_labels() %>% bold_p(), # aHR
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
#     mod.final %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p(), # aHR
#     mod.late %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p() # Late deaths
#   ),
#   tab_spanner = c("Crude estimate", "Adjusted estimate", "Late deaths")
# )
# 
# write_rds(tab_app, "dataset/tab_app.rds")

tab_app <- read_rds("dataset/tab_app.rds")

# predictions & curves ----------------------------------------------------

newdat <- expand.grid(
  exposure = levels(analytical$exposure),
  SexF = levels(analytical$SexF),
  # SexF = "Male",
  Race = "White",
  AGE = round(mean(analytical$AGE, na.rm = TRUE)),
  PROBLEMUse = "No",
  EDUCATION = "Greater Than High School",
  EMPLOYMENT = "Employed",
  RURALdc = "Urban",
  SCI = "No",
  # Cause = "Vehicular",
  ResDis = "Private Residence",
  DAYStoREHABdc = round(mean(analytical$DAYStoREHABdc, na.rm = TRUE)),
  FIMMOTD = round(mean(analytical$FIMMOTD, na.rm = TRUE)),
  FIMCOGD = round(mean(analytical$FIMCOGD, na.rm = TRUE)),
  RehabPay1 = "Private Insurance"
  )
rownames(newdat) <- letters[1:10]
