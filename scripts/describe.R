# setup -------------------------------------------------------------------

# library(skimr) # skim
library(gtsummary)
library(gt)

# setup gtsummary theme

theme_ff_gtsummary()
theme_gtsummary_compact()
# theme_gtsummary_language(language = "pt") # traduzir

# exploratory -------------------------------------------------------------

# overall description
# analytical %>%
#   skimr::skim()

# tables ------------------------------------------------------------------

# tab_desc <- analytical %>%
#   tbl_summary(
#     include = -id,
#     # by = exposure,
#   ) %>%
#   # modify_caption(caption = "**Tabela 1** Características demográficas") %>%
#   # modify_header(label ~ "**Características dos pacientes**") %>%
#   bold_labels() %>%
#   modify_table_styling(columns = "label", align = "center")
# 
# write_rds(tab_desc, "dataset/tab_desc.rds")

tab_desc <- read_rds("dataset/tab_desc.rds")
