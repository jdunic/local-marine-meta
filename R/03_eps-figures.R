# For italics see SO answer: https://stackoverflow.com/questions/27886766/r-ggplot2-varying-font-sizes-in-legend


## FIGURE 1ab
dev.new(height = 3.2, width = 7.9)
# no-event-data-plotted-with-duration-var-weighted-fig1
duration_w_nb <- duration_w + theme(plot.background = element_blank())
impact_coefs_plot_nb <- impact_coefs_plot + theme(plot.background = element_blank())
# impacts-var-weighted-intercept-coefficient-estimates-fig2
text_a <- textGrob('a)', vjust = -10.5, hjust = -1, gp = gpar(fontface = 'italic'))
text_b <- textGrob('b)', vjust = -10.5, hjust = -0.3, gp = gpar(fontface = 'italic'))
figure1 <- textGrob('Figure 1', hjust = 0)
grid.arrange(text_a, duration_w_nb, text_b, impact_coefs_plot_nb, figure1, widths = c(0.1, 1, 0.1, 1.2), heights = c(1, 0.1))

dev.copy2pdf(file = '../figures/Figure_1.pdf')



## Figure 2
dev.new(height = 5.3, width = 8.5)
# drivers-var-weighted-intercept-scaled-par-est-plot-short-long-breakdown
text_a <- textGrob('a)', vjust = -19, hjust = -0.5, gp = gpar(fontface = 'italic'))
text_b <- textGrob('b)', vjust = -19, hjust = -0.5, gp = gpar(fontface = 'italic'))
figure2 <- textGrob('Figure 2', hjust = -0.2)
grid.arrange(text_a, driver_summary_plot_short, text_b, driver_summary_plot_long, 
             figure2, ncol = 4, widths=c(0.01, 1, 0.01, 1.2), heights = c(1, 0.1))

dev.copy2pdf(file = '../figures/Figure_2.pdf')

## Figure 3
dev.new(width = 8.5, height = 5.5)
#
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
#
beepr::beep()
#
dev.copy2pdf(file = '../figures/Figure_3.pdf', width = 8.5, height = 5.5)