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
dev.new(height = 3.2, width = 9)
# driver-prediction-plots-0-max-in-dataset
master_layout <- 
grid.layout(nrow = 3, ncol = 5, 
            widths = unit(c(0.2, 1, 0.9, 0.9, 0.9), "null"),
            heights = unit(c(1, 0.1, 0.1), "null"))
#dev.new(height = 3.5, width = 11)

grid.newpage()
pushViewport(viewport(layout = master_layout))
print(nuts_alone_predictions, vp = set_vp(1, 2))
print(invs_alone_predictions, vp = set_vp(1, 3))
print(ltc_alone_predictions, vp = set_vp(1, 4))
print(all_mean_predictions, vp = set_vp(1, 5))
#
grid.text(
    "a)", vp = viewport(layout.pos.row = 1, layout.pos.col = 2), 
    gp = gpar(fontsize = 10, fontface = 'italic'), vjust = -11, hjust = 8.5
    )
grid.text(
    "b)", vp = viewport(layout.pos.row = 1, layout.pos.col = 3), 
    gp = gpar(fontsize = 10, fontface = 'italic'), vjust = -11, hjust = 8.5
    )
grid.text(
    "c)", vp = viewport(layout.pos.row = 1, layout.pos.col = 4), 
    gp = gpar(fontsize = 10, fontface = 'italic'), vjust = -11, hjust = 8.5
    )
grid.text(
    "d)", vp = viewport(layout.pos.row = 1, layout.pos.col = 5), 
    gp = gpar(fontsize = 10, fontface = 'italic'), vjust = -11, hjust = 8.5
    )
grid.text(
    expression('  Predicted change in\nspecies richness (LRR)'), 
    vp = viewport(layout.pos.row = 1:2, layout.pos.col = 1),
    rot = 90, gp = gpar(fontsize = 11, face = 'italic'), 
    vjust = 1.3, hjust = 0.4
    )
grid.text(
    expression('Study duration (years)'), 
    vp = viewport(layout.pos.row = 2, layout.pos.col = 2:5),
    gp = gpar(fontsize = 11), hjust = 0.4, vjust = 1
    )
grid.text(
  'Figure 3', vp = viewport(layout.pos.row = 3, layout.pos.col = 1), 
  hjust = 0)

dev.copy2pdf(file = '../figures/Figure_3.pdf')

# Figure 4
dev.new(height = 3, width = 5.25)
# rate-of-change-by-taxa-vw-plot
taxa_vw_plot
grid.text(
  'Figure 4', vp = viewport(layout.pos.row = 1, layout.pos.col = 1), 
  vjust = 12, hjust = 4)

dev.copy2pdf(file = '../figures/Figure_4.pdf')

