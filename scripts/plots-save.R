# setup -------------------------------------------------------------------
height <- 14
width <- 14
units <- "cm"

# publication ready tables ------------------------------------------------

# Don't need to version these files on git
# tab_inf %>%
#   as_gt() %>%
#   as_rtf() %>%
#   writeLines(con = "report/SAR-2023-004-BH-v01-T2.rtf")

# save plots --------------------------------------------------------------

ggsave(filename = "figures/surv_uncrop.png", plot = gg.surv.uncrop, height = height, width = width, units = units)
ggsave(filename = "figures/surv.png", plot = gg.surv, height = height, width = width, units = units)
