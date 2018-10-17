setwd('Meta_analysis_ms/R')
source('00_driver_extraction_functions.R')
source('01_meta_models.R')
source('02_meta_models_no_diez.R')
source('02_meta_models_lmer.R')
source('01_taxonomic_groups.R')
source('02_sensitivity_analyses.R')
source('02_site_map.R')
source('01_driver_prediction_plots.R')
source('02_driver_distributions.R')
source('02_combined-driver-effect-plot.R')
source('01_AICc-table.R')
beep()

rmarkdown::render('figures.Rmd'); beep()
rmarkdown::render('supplementary_material.Rmd'); beep()


#---
#output: pdf_document
#header-includes:
#    - \usepackage[labelformat=empty]{caption}
#---


