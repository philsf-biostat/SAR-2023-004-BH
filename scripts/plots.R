# setup -------------------------------------------------------------------
library(survminer)

ff.col <- "steelblue" # good for single groups scale fill/color brewer
ff.pal <- "Paired"    # good for binary groups scale fill/color brewer

gg <- analytical %>%
  ggplot() +
  scale_color_brewer(palette = ff.pal) +
  scale_fill_brewer(palette = ff.pal) +
  theme_ff()

# plots -------------------------------------------------------------------

# gg.outcome <- gg +
#   geom_density(aes(outcome, fill = exposure), alpha = .8) +
#   # geom_bar(aes(outcome, fill = exposure)) +
#   xlab(attr(analytical$outcome, "label")) +
#   ylab("")

cxsf <- survfit(mod.full, newdata = newdat, conf.type = "none")
surv_cxsf <- surv_summary(cxsf, data = analytical) %>% tibble()
m_newdat <- newdat[as.character(surv_cxsf$strata), ]

## plotting data frame
surv_df <- cbind(surv_cxsf, m_newdat)

gg.surv <- surv_df %>%
  ggsurvplot_df(
    # config básica do plot
    surv.geom = geom_line,
    color = "exposure",
    # linetype = "exposure",
    linetype = "SexF",
    # config extras (optional)
    # surv.median.line = "hv",
    # risk.table = TRUE,
    censor = FALSE,
    conf.int = FALSE,
    # crop. uncrop in separate figure
    ylim = c(.85, 1),
    # labels
    title = "Effect of SES on survival",
    xlab = "Time (years)",
    # ylab = "Survival",
    surv.scale = "percent",
    # theme
    palette = "OrRd",
    ggtheme = theme_ff(),
  ) +
  theme(
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    # when SexF is added to the plot, we need to change legend position
    legend.position = "right",
  )

gg.surv.uncrop <- gg.surv +
  ylim(c(0, 1))

# cool facet trick from https://stackoverflow.com/questions/3695497 by JWilliman
# gg +
#   geom_histogram(bins = 5, aes(outcome, y = ..count../tapply(..count.., ..PANEL.., sum)[..PANEL..]), fill = ff.col) +
#   scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
#   xlab(attr(analytical$outcome, "label")) +
#   ylab("") +
#   facet_wrap(~ exposure, ncol = 2)
