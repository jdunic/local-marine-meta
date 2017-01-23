setwd('Meta_analysis_ms/R')
source('meta_models.R')
source('meta_models_no_diez.R')
source('meta_models_lmer.R')
source('taxonomic_groups.R')
source('sensitivity_analyses.R')
source('site_map.R')

rmarkdown::render('figures.Rmd')
rmarkdown::render('supplementary_material.Rmd')



