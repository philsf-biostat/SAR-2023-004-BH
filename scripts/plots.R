# setup -------------------------------------------------------------------
# library(survminer)

ff.col <- "steelblue" # good for single groups scale fill/color brewer
ff.pal <- "Paired"    # good for binary groups scale fill/color brewer

gg <- analytical %>%
  ggplot() +
  scale_color_brewer(palette = ff.pal) +
  scale_fill_brewer(palette = ff.pal) +
  theme_ff()

# plots -------------------------------------------------------------------

gg.outcome <- gg +
  geom_density(aes(outcome, fill = exposure), alpha = .8) +
  # geom_bar(aes(outcome, fill = exposure)) +
  xlab(attr(analytical$outcome, "label")) +
  ylab("")

# cool facet trick from https://stackoverflow.com/questions/3695497 by JWilliman
# gg +
#   geom_histogram(bins = 5, aes(outcome, y = ..count../tapply(..count.., ..PANEL.., sum)[..PANEL..]), fill = ff.col) +
#   scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
#   xlab(attr(analytical$outcome, "label")) +
#   ylab("") +
#   facet_wrap(~ exposure, ncol = 2)
