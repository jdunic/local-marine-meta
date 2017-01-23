## @knitr no-event-caterpillar-plot-duration
library(ggplot2)
library(grid)
library(dplyr)

# Caterpillar plot of raw no event-weighted data points
test_plot <- 
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0) %>% 
  arrange(Reference) %>% 
  mutate(plotting_id = 1:nrow(.), Reference = factor(Reference)) %>% 
  mutate(odd_even = ifelse(as.numeric(Reference) %% 2 == 0, 'even', 'odd')) %>% 
  ggplot(data = ., aes(x = plotting_id, y = yi_SppR_ROM, group = factor(id))) +
    geom_rect(data = . %>% filter(odd_even == 'odd') %>% group_by(Reference) %>% summarise(min_plot_id = min(plotting_id), max_plot_id = max(plotting_id)), 
      aes(x = NULL, y = NULL, xmin = min_plot_id - 0.5, xmax = max_plot_id + 0.5, ymin = -2.7, ymax = 2.7, group = NULL), alpha = 0.3) +
    geom_hline(yintercept = 0, colour = 'grey70') +
    geom_errorbar(aes(ymin = yi_SppR_ROM - 2 * sqrt(vi_SppR_ROM), 
                      ymax = yi_SppR_ROM + 2 * sqrt(vi_SppR_ROM)), 
                  width = 0, colour = 'grey40') + 
    geom_point(aes(fill = Reference, size = 1 / vi_SppR_ROM), shape = 21, alpha = 0.7) + 
    theme_minimal() + 
    theme(axis.text = element_blank(), 
          axis.title.y = element_blank(), 
          plot.margin = unit(c(0.2, 0.2, 0.2, 4), 'cm')) + 
    xlim(0, 148) +
    guides(fill = FALSE, size = FALSE) + 
    scale_size(range = c(1.5, 10)) + 
    scale_x_continuous(expand = c(0.01, 0)) + #scale_y_continuous(expand = c(0, 0)) + 
    xlab('Log Ratio') + 
    coord_flip()
test <- 
  no_event %>% 
    filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0) %>% 
    arrange(Reference) %>% 
    mutate(plotting_id = 1:nrow(.), Reference = factor(Reference)) %>% 
    group_by(Reference) %>% 
    summarise(mean_plotting_id = mean(plotting_id))
for (i in 1:nrow(test))  {
  test_plot <- test_plot + annotation_custom(
      grob = textGrob(label = test$Reference[i], hjust = 1, vjust = 0.5, gp = gpar(cex = 0.8)),
      ymin = -2.7,    # Vertical position of the textGrob
      ymax = -2.7,
      xmin = test$mean_plotting_id[i],  # Note: The grobs are positioned outside the plot area
      xmax = test$mean_plotting_id[i]
      )
 }    
# Code to override clipping
grid.newpage()
gt <- ggplot_gtable(ggplot_build(test_plot))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

#dev.new(height = 20, width = 6.6)

#dev.copy2pdf(file = "/Users/jillian/Desktop/Rplot1.pdf")

## @knitr Duration-frequency-no-event-data
no_event %>% 
  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0) %>%
  ggplot(data = ., aes(x = Duration)) + 
    geom_histogram() + 
    theme_minimal() + 
    ylab('Frequency\n')

#no_event %>% 
#  filter(!is.na(yi_SppR_ROM) & !is.na(vi_SppR_ROM) & vi_SppR_ROM > 0) %>%
#  ggplot(data = ., aes(x = Duration)) + 
#    stat_ecdf() + 
#    theme_minimal() + 
#    geom_vline(xintercept = 15) + 
#    geom_hline(yintercept = 0.825) + 
#    ylab('Cumulative Frequency\n')
