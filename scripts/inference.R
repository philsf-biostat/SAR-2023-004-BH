# setup -------------------------------------------------------------------
# library(infer)

# tables ------------------------------------------------------------------

# tab_inf <- tbl_merge(
#   tbls = list(
#     mod.crude %>% tbl_regression(exp = TRUE, include = exposure) %>% bold_labels() %>% bold_p(), # crude HR
#     mod.social %>% tbl_regression(exp = TRUE, include = exposure) %>% modify_footnote(estimate ~ "Adjusted by demographic variables") %>% bold_labels() %>% bold_p(), # aHR
#     mod.social.clinical %>% tbl_regression(exp = TRUE, include = exposure) %>% modify_footnote(estimate ~ "Adjusted by demographic + clinical variables") %>% bold_labels() %>% bold_p(), # aHR
#     mod.final %>% tbl_regression(exp = TRUE, include = exposure) %>% modify_footnote(estimate ~ "Adjusted by demographic + clinical + geographical variables") %>% bold_labels() %>% bold_p() # aHR
#     # mod.late %>% tbl_regression(exp = TRUE, include = exposure) %>% modify_footnote(update = list(estimate = "Test")) %>% bold_labels() %>% bold_p() # Late deaths
#   ),
#   tab_spanner = c("Crude estimate", "Model 2", "Model 3", "Model 4")
# )
# 
# write_rds(tab_inf, "dataset/tab_inf.rds")
# 
# tab_app <- tbl_merge(
#   tbls = list(
#     mod.crude %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p(), # crude HR
#     mod.social %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p(), # aHR
#     mod.social.clinical %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p(), # aHR
#     mod.final %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p() # aHR
#     # mod.late %>% tbl_regression(exp = TRUE) %>% bold_labels() %>% bold_p() # Late deaths
#   ),
#   tab_spanner = c("Crude estimate", "Model 2", "Model 3", "Model 4")
# )
# 
# write_rds(tab_app, "dataset/tab_app.rds")

tab_inf <- read_rds("dataset/tab_inf.rds")
tab_app <- read_rds("dataset/tab_app.rds")

# obsolete code -----------------------------------------------------------

# tab_desc %>%
#   as_gt() %>%
#   gtsave("table1.png")
# tab_inf %>%
#   as_gt() %>%
#   gtsave("table2.png")
