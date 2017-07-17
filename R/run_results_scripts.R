setwd('Meta_analysis_ms/R')
source('driver_extraction_functions.R')
source('meta_models.R')
beep()
source('meta_models_no_diez.R')
beep()
source('meta_models_lmer.R')
beep()
source('taxonomic_groups.R')
beep()
source('sensitivity_analyses.R')
beep()
source('site_map.R')
beep()
source('driver_prediction_plots.R')
beep()
source('driver_distributions.R')
source('AICc-table.R')
beep()

rmarkdown::render('figures.Rmd'); beep()
rmarkdown::render('supplementary_material.Rmd'); beep()


#---
#output: pdf_document
#header-includes:
#    - \usepackage[labelformat=empty]{caption}
#---


