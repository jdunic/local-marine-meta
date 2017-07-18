
#install.packages("vegan")
library(vegan)
data(dune)
data(dune.env)
dune.ca <- cca(dune)
point_cols <-  rep(c('red', 'blue', 'green', 'purple'), each = 4)
plot(dune.ca, display = "sites", type = "p")
points(col = point_cols)
with(dune.env, ordiellipse(dune.ca, Management, kind = "se", conf = 0.95))
with(dune.env, ordispider(dune.ca, Management, col = "blue", label= TRUE))
with(dune.env, ordihull(dune.ca, Management, col="blue", lty=2))

par(mfrow=c(1, 3))
plot(fitted(mod1), rstandard(mod1)$z, pch=19)
plot(fitted(impact_ne_w), rstandard(impact_ne_w)$z, pch=19)
plot(fitted(drivers_scaled), rstandard(drivers_scaled)$z, pch=19)

par(mfrow=c(3,1))
profile(mod1, sigma2=1)
profile(impact_ne_w, sigma2=1)
profile(drivers_scaled, sigma2=1)

# No change in shannon diversity
mod1_shan <- 
  rma.mv(yi = yi_Shan_ROM, V = vi_Shan_ROM, 
         data = no_event %>% filter(!is.na(yi_Shan_ROM)), 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mod1_shan

# Gains over time in shannon
# Gains with higher impacts
# Impacts over time decreases evenness
impact_ne_w_shan <- 
  rma.mv(yi = yi_Shan_ROM, V = vi_Shan_ROM, 
         data = no_event,
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
impact_ne_w_shan

# Short term LTC increases shannon
# Short term nutrient increases shannon
# Long term LTC decreases shannon
# Long term Nutrients decreases shannon
drivers_scaled_shan <- 
  rma.mv(yi = yi_Shan_ROM, V = vi_Shan_ROM, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
drivers_scaled_shan

# No change in shannon diversity
mod1_ss_shan <- 
  rma.mv(yi = yi_Shan_ROM, V = 1 / n1, 
         data = no_event, 
         random = ~ 1 | as.factor(Study.ID), 
         mods = ~ Duration)
mod1_ss_shan

# No change in shannon
impact_ne_ss_shan <- 
  rma.mv(yi = yi_Shan_ROM, V = 1 / n1, 
         data = no_event,
         random = ~ 1 | factor(Study.ID), 
         mods = ~ Duration * mean_imps)
impact_ne_ss_shan

# No change in shannon
drivers_scaled_ss_shan <- 
  rma.mv(yi = yi_Shan_ROM, V = 1 / n1, 
         data = no_event %>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (scale(scaled_invs) + scale(sliced_ltc) + scale(mean_nuts)))
drivers_scaled_ss_shan