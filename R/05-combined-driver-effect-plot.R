#Libraries
library(tidyverse)
library(ggplot2)
library(metafor)



#the data
source('00_functions.R')

fl_combined <- readr::read_csv("../Data_outputs/fl_combined.csv") %>% 
  mutate(Study.ID = factor(Study.ID)) %>% 
  # This study was a duplicate
  filter(Study.ID != 'Shimanaga') %>% 
  # Keller study 
  filter(Study.ID != '172') %>%
  # Study  136 - Enfermeria should have been classified as having an event - 
  # 'shrimp farming' and 'tidal restriction'
  filter(Site != 'Enfermeria')

no_event2 <- filter(fl_combined, Event != 'Yes')

#The model
drivers_unscaled <- 
  rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, 
         data = no_event2, #%>% mutate(scaled_invs = mean_invs * 10^-3), 
         random = ~ 1 | Study.ID, 
         mods = ~ Duration * (mean_invs + sliced_ltc + mean_nuts))

#Now setup conditions for  predictions
temp = c(-1, -0.5, -0.01, 0.01, 0.5, 1)
invs = seq(from = 0, to = 160000, by = 1000)
duration = as.vector(c(5, 10,15, 20), mode = 'integer')
nuts = seq(from = 0, to = 200, by = 2)

#make the predictions
prediction_df <- 
  crossing(duration, invs, temp, nuts) %>% 
  mutate(invs_dur = invs*duration, temp_dur = temp*duration, nuts_dur = nuts*duration)  
prediction_mat <- (as.matrix(prediction_df))
dimnames(prediction_mat) <- NULL

#format for plotting
g_preds_raw <- 
  bind_cols(prediction_df, 
            predict.rma(object = drivers_unscaled, newmods = prediction_mat) %>%
              as_data_frame())
beepr::beep()

g_preds <- g_preds_raw %>%
  mutate(change = case_when(ci.ub < 0 ~ 'Loss', ci.lb > 0 ~ 'Gain', ci.lb < 0 & ci.ub > 0 ~ 'No change')) %>%
  mutate(change = factor(change, level = c('Gain', 'No change', 'Loss'))) %>%
  mutate(duration = str_c(duration, "years", sep=" ")) %>%
  mutate(duration = factor(duration, levels = c('5 years', '10 years', 
                                                '15 years', '20 years')))

# %>%
#   
#   mutate(nuts_factor = case_when(nuts == nut_quantiles$`0%` ~ 'low (0)', 
#                                  nuts == nut_quantiles$`50%` ~ 'median (0.46)', 
#                                  nuts == nut_quantiles$`100%` ~ 'max (185)')) %>% 
#   mutate(temp = case_when(temp <=  -0.5 ~ '< -0.5', 
#                           temp >   -0.5 & temp < 0 ~ '-0.5 to 0',
#                           temp >    0 & temp < 0.5 ~ '0 to -0.5', 
#                           temp >=   0.5 ~ '> 0.5')) %>% 
#   mutate(temp = factor(temp, levels = c('< -0.5', 
#                                         '-0.5 to 0', 
#                                         '0 to -0.5',
#                                         '> 0.5'))) %>% 
#   mutate(duration = case_when(duration == 5 ~ '5 years', 
#                               duration == 20 ~ '20 years')) %>% 

#Plot!
dev.new(width = 11, height = 5)

ggplot() + 
  theme(legend.background = element_blank(), 
        legend.key = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        strip.background = element_blank(), 
        plot.background = element_blank(),
        strip.text.y = element_text(angle = 0), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 10)) + 
  geom_raster(data = filter(g_preds), aes(x = invs, y = nuts, 
                                          fill = change, alpha=abs(pred)), 
              interpolate=TRUE) + 
  scale_fill_manual(values = c('#0571b0', 'grey90', '#ca0020'),
                    guide = guide_legend("Direction of\nRichness Change")) + 
  scale_alpha(guide = guide_legend("Absolute\nMagnitude\n(LRR)")) +
  facet_grid(duration ~ temp) + 
  xlab('\n   Invasion potential') + 
  ylab('Nutrient use\n') + 
  labs(colour = 'LRR') + 
  guides(colour = guide_legend(override.aes = list(size = 3))) + 
  ggtitle(expression("   Temperature Change ("*degree*"C)"))

beepr::beep()

dev.copy2pdf(file = '../figures/3_new_combined-drivers.pdf', width = 8, height = 5)


####### marignal effects
library(modelr)

make_marg_data <- function(avar){
  avar_x <- enquo(avar)
  name_x <- quo_name(avar_x)
  print(avar_x)
  print(name_x)
  # vars <- quos(mean_invs, sliced_ltc, mean_nuts)
  
  dat <- no_event2 %>% data_grid(
                    Duration = seq_range(Duration, 3),
                    !!name_x := seq_range(!!avar_x, 200))
  
  vars <- c("mean_invs", "sliced_ltc", "mean_nuts")
  vars <- vars[vars!=name_x]
  
  values <- map(vars, ~median(no_event2[[.]], na.rm=T))
  names(values) <- vars
  
  cbind(dat, values, list(var = name_x))
} 



marg_data_frame <- bind_rows(
  make_marg_data(mean_invs),
  make_marg_data(sliced_ltc),
  make_marg_data(mean_nuts)
)

pred_frame <- as_tibble(predict(drivers_unscaled, newdata = marg_data_frame))
  
  