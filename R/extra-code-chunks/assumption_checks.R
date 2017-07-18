test <- data_frame(QM =  c(mod1$QM, impact_ne_w$QM, drivers_scaled$QM), 
                   QMp = c(mod1$QMp, impact_ne_w$QMp, drivers_scaled$QMp), 
                   QE =  c(mod1$QE, impact_ne_w$QE, drivers_scaled$QE), 
                   QEp = c(mod1$QEp, impact_ne_w$QEp, drivers_scaled$QEp))


random_mod <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event %>% filter(!is.na(yi_SppR_ROM)), 
         random = ~ 1 | as.factor(Study.ID))
random_mod

(mod1$sigma2[1] - random_mod$sigma2[1]) / mod1$sigma2[1]
(impact_ne_w$sigma2[1] - random_mod$sigma2[1]) / impact_ne_w$sigma2[1]
(drivers_scaled$sigma2[1] - random_mod$sigma2[1]) / drivers_scaled$sigma2[1]

## @knitr funnel-plot-mod1
par(mfrow = c(2, 2))
funnel(mod1, main="Standard Error")
funnel(mod1, yaxis="vi", main="Sampling Variance")
funnel(mod1, yaxis="seinv", main="Inverse Standard Error")
funnel(mod1, yaxis="vinv", main="Inverse Sampling Variance")

## @knitr funnel-plot-impact_ne_w
par(mfrow = c(2, 2))
funnel(impact_ne_w, main="Standard Error")
funnel(impact_ne_w, yaxis="vi", main="Sampling Variance")
funnel(impact_ne_w, yaxis="seinv", main="Inverse Standard Error")
funnel(impact_ne_w, yaxis="vinv", main="Inverse Sampling Variance")

## @knitr funnel-plot-drivers_scaled
par(mfrow = c(2, 2))
funnel(drivers_scaled, main="Standard Error")
funnel(drivers_scaled, yaxis="vi", main="Sampling Variance")
funnel(drivers_scaled, yaxis="seinv", main="Inverse Standard Error")
funnel(drivers_scaled, yaxis="vinv", main="Inverse Sampling Variance")

## @knitr rma-qqplots
par(mfrow = c(1, 3))
car::qqPlot(rstandard(mod1)$z, xlab='Theoretical quantiles', ylab='Sample quantiles')
car::qqPlot(rstandard(impact_ne_w)$z, xlab='Theoretical quantiles', ylab='Sample quantiles')
car::qqPlot(rstandard(drivers_scaled)$z, xlab='Theoretical quantiles', ylab='Sample quantiles')
#car::qqPlot(rstandard(drivers_unscaled)$z)

## @knitr rma-profiles
par(mfrow = c(1, 3))
profile(mod1)
profile(impact_ne_w)
profile(drivers_scaled)


## @knitr rma-residual-plots
# Duration residuals
plot(fitted(mod1)[which(!is.na(no_event$vi_SppR_ROM))], rstandard(mod1)$z, 
     xlab = 'Fitted values', ylab = 'Standardised residuals')
abline(h = 0, col = 'red', lty = 2)
# Impacts residuals
plot(fitted(impact_ne_w)[which(!is.na(no_event$vi_SppR_ROM) & !is.na(no_event$mean_imps))], rstandard(impact_ne_w)$z, 
     xlab = 'Fitted values', ylab = 'Standardised residuals')
abline(h = 0, col = 'red', lty = 2)
# Drivers residuals
plot(fitted(drivers_scaled)[which(!is.na(no_event$vi_SppR_ROM) & !is.na(no_event$sliced_ltc) & !is.na(no_event$mean_nuts) & !is.na(no_event$mean_invs))], rstandard(drivers_scaled)$z, 
     xlab = 'Fitted values', ylab = 'Standardised residuals')
abline(h = 0, col = 'red', lty = 2)



#-------------------------------------------------------------------------------
# Taxonomic checks

## @knitr rma-taxa-qqplots
par(mfrow = c(1, 5))
car::qqPlot(rstandard(mixed_vw)$z, xlab = 'Fitted values', ylab = 'Standardised residuals')
car::qqPlot(rstandard(fish_vw)$z, xlab = 'Fitted values', ylab = 'Standardised residuals')
car::qqPlot(rstandard(algae_vw)$z, xlab = 'Fitted values', ylab = 'Standardised residuals')
car::qqPlot(rstandard(zooplankton_vw)$z, xlab = 'Fitted values', ylab = 'Standardised residuals')
car::qqPlot(rstandard(inverts_vw)$z, xlab = 'Fitted values', ylab = 'Standardised residuals')
