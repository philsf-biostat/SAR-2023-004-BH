# setup -------------------------------------------------------------------

# figure dimensions (1366 x 743)
w <- 455
h <- 371

# time split (years)
cutpoint <- 1

md <- md %>%
  mutate(
    status1 = ifelse(Time < cutpoint, outcome, 0),
    status2 = ifelse(Time >=cutpoint, outcome, 0),
  )

# function to inspect Schoenfeld test
sch <- function(x, sort = TRUE) {
  if(class(x) != "cox.zph") stop("Not a Schoenfeld residuals object!")

  x <- x$table %>% as.data.frame()

  # sort p-values or identify terms
  if(sort) {
    x <- x %>%
      arrange(p)
  } else {
    x <- x %>%
      rownames_to_column(var = "term")
  }

  # format output
  x %>% mutate(p = style_pvalue(p))
}

# diagnostics -------------------------------------------------------------

# full model
# Schoenfeld residuals
res.full.sch <- cox.zph(mod.full)
# (cox.zph(mod.full))$table %>% as.data.frame() %>% arrange(p) %>% mutate(p = style_pvalue(p))
sch(res.full.sch)

png("figures/diag_full-sch.png", h = h, w = 3*w)
par(mfrow=c(1,3))
plot(cox.zph(mod.full, transform = "identity")[13])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod.full, transform = "identity")[14])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod.full, transform = "identity")[15])
abline(h=0, col = "red", lwd = 2)
dev.off()

res.full.m <- resid(mod.full, type = "martingale")
res.full.d <- resid(mod.full, type = "deviance")
res.full.s <- resid(mod.full, type = "dfbeta")

# Martingale residuals
png("figures/diag_full-mar.png")
plot(res.full.m, ylab = "Martingale residuals")
lines(lowess(res.full.m, iter = 0), lty = 2, col = "blue", lwd = 2)
# identify(res.full.m)
dev.off()

# null model --------------------------------------------------------------

# saturated & null
mod.null <- coxph(Surv(Time, outcome) ~ 1, md)
# mod.sat <- coxph(Surv(Time, outcome) ~ id, md)

res.null.m <- resid(mod.null, type = "martingale")
# res.sat.m <- resid(mod.sat, type = "martingale")

# Martingale: covariates against null -------------------------------------

plot(md$Cause, res.null.m, ylab = "Martingale residuals")
lines(lowess(md$Cause, res.null.m, iter = 0), lty = 2, col = "blue", lwd = 2)

png("figures/diag_null-mar.png", h = h, w = 3*w)
par(mfrow = c(1,3))
plot(md$FIMMOTD, res.null.m, ylab = "Martingale residuals")
lines(lowess(md$FIMMOTD, res.null.m, iter = 0), lty = 2, col = "blue", lwd = 2)
plot(md$FIMCOGD, res.null.m, ylab = "Martingale residuals")
lines(lowess(md$FIMCOGD, res.null.m, iter = 0), lty = 2, col = "blue", lwd = 2)
plot(md$DAYStoREHABdc, res.null.m, ylab = "Martingale residuals")
lines(lowess(md$DAYStoREHABdc, res.null.m, iter = 0), lty = 2, col = "blue", lwd = 2)
abline(v = 30, lty = 2, lwd = 2) # non-linear peak
# identify(res.null.m)
dev.off()

# time split --------------------------------------------------------------

mod1.full <- update(mod.full, Surv(Time, status1) ~ .)
mod2.full <- update(mod.full, Surv(Time, status2) ~ .)

res.full1.sch <- cox.zph(mod1.full)
res.full2.sch <- cox.zph(mod2.full)

sch(res.full1.sch)
sch(res.full2.sch)

png("figures/diag_full_split-sch.png", h = 2*h, w = 3*w)
par(mfrow=c(2,3))
plot(cox.zph(mod1.full, transform = "identity")[13])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod1.full, transform = "identity")[14])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod1.full, transform = "identity")[15])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod2.full, transform = "identity")[13])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod2.full, transform = "identity")[14])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod2.full, transform = "identity")[15])
abline(h=0, col = "red", lwd = 2)
dev.off()

# strata ------------------------------------------------------------------

# mod.strat <- update(mod.full, . ~ . -Cause + strata(Cause))
mod.strat <- coxph(Surv(Time, outcome) ~ exposure +
                     SexF +
                     Race +
                     AGE +
                     PROBLEMUse +
                     EDUCATION +
                     EMPLOYMENT +
                     RURALdc +
                     SCI +
                     strata(Cause) +
                     RehabPay1 +
                     ResDis +
                     DAYStoREHABdc +
                     FIMMOTD +
                     FIMCOGD,
                   md)

# (cox.zph(mod.strat))$table %>% as.data.frame() %>% arrange(p) %>% mutate(p = style_pvalue(p))
res.strat.sch <- cox.zph(mod.strat)

sch(res.strat.sch)

png("figures/diag_strat-sch.png", h = h, w = 3*w)
par(mfrow=c(1,3))
plot(cox.zph(mod.strat, transform = "identity")[12])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod.strat, transform = "identity")[13])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod.strat, transform = "identity")[14])
abline(h=0, col = "red", lwd = 2)
dev.off()

res.strat.m <- resid(mod.strat, type = "martingale")
res.strat.d <- resid(mod.strat, type = "deviance")
res.strat.s <- resid(mod.strat, type = "dfbeta")

png("figures/diag_strat-mar.png")
plot(res.strat.m, ylab = "Martingale residuals")
lines(lowess(res.strat.m, iter = 0), lty = 2, col = "blue", lwd = 2)
# identify(res.strat.m)
dev.off()

# strata + time split -----------------------------------------------------

# mod1.strat <- coxph(Surv(Time, status1) ~ exposure + SexF + Race + AGE + PROBLEMUse + EDUCATION + EMPLOYMENT + RURALdc + SCI + strata(Cause) + RehabPay1 + ResDis + DAYStoREHABdc + FIMMOTD + FIMCOGD, md)
# mod2.strat <- coxph(Surv(Time, status2) ~ exposure + SexF + Race + AGE + PROBLEMUse + EDUCATION + EMPLOYMENT + RURALdc + SCI + strata(Cause) + RehabPay1 + ResDis + DAYStoREHABdc + FIMMOTD + FIMCOGD, md)
mod1.strat <- update(mod.strat, Surv(Time, status1) ~ .)
mod2.strat <- update(mod.strat, Surv(Time, status2) ~ .)

# (cox.zph(mod1.strat))$table %>% as.data.frame() %>% arrange(p) %>% mutate(p = style_pvalue(p))
# (cox.zph(mod2.strat))$table %>% as.data.frame() %>% arrange(p) %>% mutate(p = style_pvalue(p))

res.strat1.sch <- cox.zph(mod1.strat)
res.strat2.sch <- cox.zph(mod2.strat)

sch(res.strat1.sch)
sch(res.strat2.sch)

png("figures/diag_strat_split-sch.png", h = 2*h, w = 3*w)
par(mfrow=c(2,3))
plot(cox.zph(mod1.strat, transform = "identity")[12])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod1.strat, transform = "identity")[13])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod1.strat, transform = "identity")[14])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod2.strat, transform = "identity")[12])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod2.strat, transform = "identity")[13])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod2.strat, transform = "identity")[14])
abline(h=0, col = "red", lwd = 2)
dev.off()

# pspline -----------------------------------------------------------------

mod.sp <- coxph(Surv(Time, outcome) ~ exposure +
                  SexF +
                  Race +
                  AGE +
                  PROBLEMUse +
                  EDUCATION +
                  EMPLOYMENT +
                  RURALdc +
                  SCI +
                  strata(Cause) +
                  RehabPay1 +
                  ResDis +
                  pspline(DAYStoREHABdc) +
                  pspline(FIMMOTD) +
                  pspline(FIMCOGD),
                md)
# (cox.zph(mod.sp))$table %>% as.data.frame() %>% arrange(p) %>% mutate(p = style_pvalue(p))
res.sp.sch <- cox.zph(mod.sp)
sch(res.sp.sch)

png("figures/diag_spline-sch.png", h = h, w = 3*w)
par(mfrow=c(1,3))
plot(cox.zph(mod.sp, transform = "identity")[12])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod.sp, transform = "identity")[13])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod.sp, transform = "identity")[14])
abline(h=0, col = "red", lwd = 2)
dev.off()

res.sp.m <- resid(mod.sp, type = "martingale")
res.sp.d <- resid(mod.sp, type = "deviance")
res.sp.s <- resid(mod.sp, type = "dfbeta")

png("figures/diag_spline-mar.png", h=h, w=w)
plot(res.sp.m, ylab = "Martingale residuals")
lines(lowess(res.sp.m, iter = 0), lty = 2, col = "red", lwd = 2)
# identify(res.sp.m)
dev.off()

# strata + pspline + time split -------------------------------------------

mod1.sp <- update(mod.sp, Surv(Time, status1) ~ .)
mod2.sp <- update(mod.sp, Surv(Time, status2) ~ .)

res.sp1.sch <- cox.zph(mod1.sp)
res.sp2.sch <- cox.zph(mod2.sp)

# (cox.zph(mod1.sp))$table %>% as.data.frame() %>% arrange(p) %>% mutate(p = style_pvalue(p))
# (cox.zph(mod2.sp))$table %>% as.data.frame() %>% arrange(p) %>% mutate(p = style_pvalue(p))

sch(res.sp1.sch)
sch(res.sp2.sch)

png("figures/diag_spline_split-sch.png", h=2*h, w=3*w)
par(mfrow=c(2,3))
plot(cox.zph(mod1.sp, transform = "identity")[12])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod1.sp, transform = "identity")[13])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod1.sp, transform = "identity")[14])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod2.sp, transform = "identity")[12])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod2.sp, transform = "identity")[13])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod2.sp, transform = "identity")[14])
abline(h=0, col = "red", lwd = 2)
dev.off()

# FIM scores var density
gg + geom_density(aes(FIMMOTD), fill = ff.col)
gg + geom_density(aes(FIMCOGD), fill = ff.col)

# log transform -----------------------------------------------------------

# days2rehab may like to see things through log lenses
gg + geom_density(aes(DAYStoREHABdc), fill = ff.col)
gg + geom_density(aes(log(DAYStoREHABdc)), fill = ff.col)

mod.sp.tr <- update(mod.sp, . ~ . -pspline(DAYStoREHABdc) + log(DAYStoREHABdc))
anova(mod.sp, mod.sp.tr) # not quite, worth trying

sch(cox.zph(mod.sp.tr))

# polynomial --------------------------------------------------------------

mod.poly <- update(mod.full, . ~ . -FIMMOTD + poly(FIMMOTD, 5))

res.poly.sch <- cox.zph(mod.poly)
sch(res.poly.sch)

png("figures/diag_poly-sch.png", h = h, w = 3*w)
par(mfrow=c(1,3))
plot(cox.zph(mod.poly, transform = "identity")[13])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod.poly, transform = "identity")[14])
abline(h=0, col = "red", lwd = 2)
plot(cox.zph(mod.poly, transform = "identity")[15])
abline(h=0, col = "red", lwd = 2)
dev.off()

res.poly.m <- resid(mod.poly, type = "martingale")
res.poly.d <- resid(mod.poly, type = "deviance")
res.poly.s <- resid(mod.poly, type = "dfbeta")

png("figures/diag_poly-mar.png", h=h, w=w)
plot(res.poly.m, ylab = "Martingale residuals")
lines(lowess(res.poly.m, iter = 0), lty = 2, col = "red", lwd = 2)
# identify(res.poly.m)
dev.off()

# Interaction with time ---------------------------------------------------

# full
mod.full.time <- update(mod.full, . ~ . + DAYStoREHABdc:Time + FIMMOTD:Time + FIMCOGD:Time)
res.full.time.sch <- cox.zph(mod.full.time)
sch(res.full.time.sch)

par(mfrow = c(2,3))
plot(cox.zph(mod.full.time, transform = "identity")[c(13:18)])
dev.off()

# strata
mod.strat.time <- update(mod.strat, . ~ . + DAYStoREHABdc:Time + FIMMOTD:Time + FIMCOGD:Time)
res.strat.time.sch <- cox.zph(mod.strat.time)
sch(res.strat.time.sch)

par(mfrow = c(2,3))
plot(cox.zph(mod.strat.time, transform = "identity")[c(12:17)])
dev.off()

# spline
mod.sp.time <- update(mod.sp, . ~ . + FIMMOTD:Time + FIMCOGD:Time + DAYStoREHABdc:Time)
# mod.sp.sptime <- update(mod.sp, . ~ . + pspline(FIMMOTD):Time + pspline(FIMCOGD):Time + pspline(DAYStoREHABdc):Time)

res.sp.time.sch <- cox.zph(mod.sp.time)
# res.sp.sptime.sch <- cox.zph(mod.sp.sptime)

sch(res.sp.time.sch)
# sch(res.sp.sptime.sch)

plot(cox.zph(mod.sp.time, transform = "identity")[15])
# plot(cox.zph(mod.sp.sptime, transform = "identity")[15])


