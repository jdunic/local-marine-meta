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
  mutate(duration = stringr::str_c(duration, "years", sep=" ")) %>%
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
dev.new(width = 8.5, height = 5.5)

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
                    guide = guide_legend("Direction of\nrichness change")) + 
  scale_alpha(guide = guide_legend("Absolute\nmagnitude\n(LRR)")) +
  facet_grid(duration ~ temp) + 
  xlab('\n   Invasion potential') + 
  ylab('Nutrient use\n') + 
  labs(colour = 'LRR') + 
  guides(colour = guide_legend(override.aes = list(size = 3))) + 
  ggtitle(expression("   Temperature change ("*degree*"C)"))
grid.text('Figure 3', hjust = 6.75, vjust = 22.5)

beepr::beep()

dev.copy2pdf(file = '../figures/Figure_3.pdf', width = 8.5, height = 5.5)


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
  
######## A little animation
library(gganimate)
theme_set( theme_bw(base_size=17) +
             theme(legend.background = element_blank(), 
                   legend.key = element_blank(), 
                   panel.background = element_blank(), 
                   panel.border = element_blank(), 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(), 
                   strip.background = element_blank(), 
                   plot.background = element_blank(),
                   strip.text.y = element_text(angle = 0), 
                   axis.text.x = element_text(angle = 45, hjust = 1)))
#make the predictions
surface_df <- no_event2 %>%
  data_grid(duration = 20,
            invs = seq_range(invs, 101),
            temp = seq(-1,1,length.out = 101),
            nuts = seq_range(nuts, 101))
  crossing(duration, invs, temp, nuts) 
  
surface_mat <- surface_df %>% 
  mutate(invs_dur = invs*duration, temp_dur = temp*duration, nuts_dur = nuts*duration) %>%
  as.matrix()
dimnames(surface_mat) <- NULL

#format for plotting
g_surface <- 
  bind_cols(surface_df, 
            predict.rma(object = drivers_unscaled, newmods = surface_mat) %>%
            as_tibble()) %>%
  mutate(change = case_when(ci.ub < 0 ~ 'Loss', ci.lb > 0 ~ 'Gain', ci.lb < 0 & ci.ub > 0 ~ 'No change')) %>%
  mutate(change = factor(change, level = c('Gain', 'No change', 'Loss'))) 

beepr::beep()


g_surface <- g_surface %>% group_by(nuts, invs) %>%
  slice(1L) %>%
  ungroup %>%
  select(nuts, invs) %>%
  mutate(pointgroup = 1:n()) %>%
  right_join(g_surface)

saveRDS(g_surface, file = "../Data_outputs/g_surface.Rds")


#g_surface <- readRDS("../Data_outputs/g_surface.Rds")

anim <- ggplot(g_surface %>% mutate(temp = factor(round(temp,2))),
       aes(x = invs, y = nuts, color = change, 
           fill = change, alpha=abs(pred),
           group = pointgroup)) + 
  geom_raster(interpolate=TRUE) + 
  scale_fill_manual(values = c('#0571b0', 'grey90', '#ca0020'),
                    guide = guide_legend("Direction of\nRichness Change")) + 
  scale_alpha(guide = guide_legend("Absolute\nMagnitude\n(LRR)")) +
  xlab('\n   Invasion potential\n(Metric tonnes cargo in 2011') + 
  ylab('Nutrient use\n(Metric tonnes N and P fertilizer from 2007-2011\n') + 
  labs(colour = 'LRR') + 
  guides(colour = guide_legend(override.aes = list(size = 3))) + 
  transition_states(temp)+
  enter_fade() +
  exit_fade() +
  ggtitle("Temperature Change:  {closest_state} deg C per decade")


animate(anim, width = 700, height = 500, nframes=250)
anim_save("../figures/surface_anim.gif")
beepr::beep()

