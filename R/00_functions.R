# General functions to make life easier

# to not use quotes
qw <- function(...) {
  sapply(match.call()[-1], deparse)
}

# Making NSE version of filter
# http://justanotherdatablog.blogspot.com/2015/03/dplyr-use-cases-non-interactive-mode.html
filter_fn <- function(d_in,filter_crit){
  d_out = d_in %>%
    filter_(filter_crit)
  return(d_out)
}

# Plotting using grid, with a set viewport
set_vp <- function(row, column) {
  viewport(layout.pos.row = row, layout.pos.col = column)
}


# Meta-analysis functions

# Back calculate log ratio into the percent change in species richness
get_percent_change <- function(log_ratio, as_percent = TRUE) {
  change = exp(log_ratio) - 1
  if (as_percent == TRUE) {
    change = change * 100
  }
  return(change)
}

# Read rich data - used in remake yml file
read_rich_data <- function(file = 'master_data/Data.csv') {
  read_csv(file, col_types = cols(Lat = col_skip(), Long = col_skip()), na = c('', 'NA', 'N/A', 'Na', 'na', ' '))
}

# Use LR ~ Duration + mean_imps + Duration:mean_imps model fitted to data to 
# make predictions for discrete cumulative human impact values over a given 
# duration.
get_imp_predictions <- function(rma_object, impact_value, duration) {
  mods = cbind(1:duration, rep(impact_value, duration), impact_value*(1:duration))
  predictions <- predict(object = rma_object, newmods = mods)
  class(predictions) <- 'list'
  predictions <- predictions[1:6]
  predictions <- as_data_frame(predictions)
  predictions$duration <- 1:duration
  return(predictions)
}

# Use LR ~ Duration * (scaled_invs + sliced_ltc + mean_nuts) model fitted to 
# data to make predictions for the effects of three different drivers on 
# species richness change over a given duration.
#
get_driver_predictions <- function(unscaled_rma_object, invs, nuts, temp, duration) {
  # invasive values were scaled by 0.001 to make invasives data variance similar
  # in magnitude to the other drivers 
  #invs <- invs * 10^-3
#
  mods = cbind(1:duration, 
               rep(invs, duration),  
               rep(temp, duration), 
               rep(nuts, duration),
               invs*(1:duration), 
               temp*(1:duration), 
               nuts*(1:duration))
  predictions <- predict(object = unscaled_rma_object, newmods = mods)
  class(predictions) <- 'list'
  predictions <- predictions[1:6]
  predictions <- as_data_frame(predictions)
  predictions$duration <- 1:duration
  return(predictions)
}

###########################################################################
#                        Make summary data frames                         #
###########################################################################

# Make a summary dataframe using an rma object so that you can get 
# mods = "string of mods column name"
# Need to fix when certain mods are dropped because the number of points is too
# small, write better num_studies and num_sites calculation)
mk_rma_summary_df <- function(original_df, rma_object, categorical = FALSE) {
  #browser()
    factors <- dimnames(rma_object$b)[[1]]
    mean_estimate <- as.vector(rma_object$b[, 1])
    lower_ci <- rma_object$ci.lb
    upper_ci <- rma_object$ci.ub
    # sigma2 in metafor is the between study variation
    tau2 <- rma_object$sigma2
    # tau2 in an rma object is the within study variation
    sigma2 <- rma_object$tau2
    pval <-rma_object$pval 
    rho <- rma_object$rho
    if(is.null(rho) == TRUE) {rho <- NA}
    percent_change <- (exp(mean_estimate) - 1) * 100

    if (categorical == TRUE) {
      num_sites <- 
        dplyr::filter(original_df, !is.na(yi.SppR.ROM) & !is.na(vi.SppR.ROM) & vi.SppR.ROM > 0) %>%
          dplyr::count(., expected_change)
      num_studies <- 
        dplyr::filter(original_df, !is.na(yi.SppR.ROM) & !is.na(vi.SppR.ROM) & vi.SppR.ROM > 0) %>% 
          dplyr::distinct(Study.ID, expected_change) %>%
          dplyr::count(., expected_change)
      adf <- data.frame(moderator = factors, mean_estimate = mean_estimate, 
                        lower_ci = lower_ci, upper_ci, pval = pval, 
                        sigma2 = sigma2, rho = rho, 
                        studies_per_mod = num_studies$n, 
                        sites_per_mod = num_sites$n, 
                        percent_change = percent_change)
    } else {
      adf <- data.frame(moderator = factors, mean_estimate = mean_estimate, 
                        lower_ci = lower_ci, upper_ci, pval = pval, 
                        sigma2 = sigma2, rho = rho, 
                        percent_change = percent_change)
      return(adf)
  }
}

mk_lme_summary_df <- function(unweighted_df, mods, lme_object) {
    factor <- dimnames(intervals(lme_object)$fixed)[[1]]
    #browser()
    df <- as.data.frame(intervals(lme_object)$fixed)
    mean_estimate <- df$est.
    lower_ci <- df$lower
    upper_ci <- df$upper
    num_studies <- ddply(unweighted_df, .(mods), summarise, 
                         studies = length(unique(Study.ID))
                         )[, 2]
    num_sites <- ddply(unweighted_df, .(mods), summarise, 
                       studies = length(unique(Site))
                       )[, 2]
    df <- data.frame(moderator = factor, mean_estimate = mean_estimate, 
                     lower_ci = lower_ci, upper_ci = upper_ci, num_studies = 
                     num_studies, num_sites = num_sites)
    cbind(df, mean_estimate, lower_ci, upper_ci, num_studies, num_sites)
    colnames(df) <- c("moderator", "mean_estimate", "lower_ci", "upper_ci", 
                      "studies_per_mod", "sites_per_mod")
    return(df)
}

###########################################################################
#                            Plotting functions                           #
###########################################################################

# Function for white on black ggplot theme - for black background presentations
theme_wb <- function() {
  theme(panel.background = element_rect(fill = 'black'), 
  plot.background = element_rect(fill = 'black'), 
  axis.line = element_line(colour = 'white'), 
  axis.text = element_text(colour = 'white'), 
  axis.title = element_text(colour = 'white'), 
  panel.grid = element_blank()
  )
}

# Make normality plots with qqline for different variables and give them a title
plot_norm <- function(variable, title) {
    qqnorm(variable, main = title)
    qqline(variable)
}

# Colour code data points that have variances associated with them
colour_var <- function(column) {
    if (is.na(column)) {
        colour <- "blue"
    } else {
        colour <- "red"
    }
}

###########################################################################
#                          Human impact functions                         #
###########################################################################

# Cumulative human impact functions
get_mean_imp <- function(impact_vector) {
  # Must unlist the list of values for a given point to remove any empty values
  # (these are not NULL, not NA, and not zero, they are just numeric vectors 
  # with length = 0)
  impact_vector <- unlist(impact_vector)
  # Replace 0 values in mean impacts with NA because land is zero
  impact_vector <- replace(impact_vector, impact_vector == 0, NA)
  mean_imp_vals <- mean(impact_vector, na.rm = TRUE)
  return(mean_imp_vals)
}

# Functions to get human impact categorical values from original colour only 
# raster (original 2008 tif).
rgb_2_hex <- function(rgb_vec) {
    if (is.na(rgb_vec[1])) {
        return(NA)
    }
    rgb(rgb_vec[1], rgb_vec[2], rgb_vec[3], maxColorValue = 255)
}

get_imp_hex <- function(x) {
    len <- dim(x)[1]
    hex_df <- character()
    for (i in 1:len) {
        hex_df[i] <- rgb_2_hex(x[i, ])
    }
    return(hex_df)
}

###########################################################################
#                          Data Cleanup Functions                         #
###########################################################################
# Revised function to get first-last values that have consistent sampling months
make_date_col <- function(year, month) {
  dates <- structure(numeric(1), class="Date")
  for (i in seq_along(year)) {
    #browser()
    if (is.na(month[i]) | month[i] == '') {
      dates[i] <- as.Date(paste0(as.character(year[i]), '/01/01'), '%Y/%m/%d')
    } else {
      dates[i] <- as.Date(paste0(as.character(year[i]), '/', month[i], '/01'), '%Y/%m/%d')
    }
  }
  return(dates)
}

# Select the first and last data points from the largest time difference 
# (+/- one month)
get_month_diff <- function(date1, date2) {
    md <- (year(date2) - year(date1)) * 12  + abs(month(date2) - month(date1))
    return(md)
}

get_first_last <- function(adf, dataset = 'richData', 
  noYCols = noYCols, y1Cols = y1Cols, y2Cols = y2Cols) {
  #browser()
  if (length(adf$date1) == 1) {
    date_ones <- adf$date1
    date_twos <- adf$date2
  } else {
    all_dates <- unique(c(adf$date1, adf$date2))
    all_date_combos <- combn(all_dates, 2)
    date_ones <- as.Date(all_date_combos[1, ], origin = "1970-01-01")
    date_twos <- as.Date(all_date_combos[2, ], origin = "1970-01-01")
  }

  month_diffs <- vector(length = length(date_ones))
  modulus <- vector(length = (length(date_ones)))

  md_df <- 
  data.frame(date1 = date_ones, date2 = date_twos, month_diff = month_diffs, 
             modulus = modulus)

  for (i in seq_along(date_ones)) {
    md_df$month_diff[i] <- get_month_diff(date_ones[i], date_twos[i])
    md_df$modulus[i] <- md_df$month_diff[i] %% 12
  }

  within_month <- 
    filter(md_df, modulus %in% c(0, 1, 11)) %>% 
    arrange(modulus)
  max_diff_id <- which.max(within_month$month_diff)
  first <- within_month$date1[max_diff_id]
  last  <- within_month$date2[max_diff_id]  
  
  if (within_month[max_diff_id, 'modulus'] == 0) {
    t_diff  <- 'perfect year' 
  } else if (within_month[max_diff_id, 'modulus'] == 1) {
    t_diff <- 'plus one month'
    } else if (within_month[max_diff_id, 'modulus'] == 11) {
      t_diff <- 'minus one month'
    }

#  perfect_year <- filter(md_df, modulus == 0)
#  max_diff_id <- which.max(perfect_year$month_diff)
#  first <- perfect_year$date1[max_diff_id]
#  last  <- perfect_year$date2[max_diff_id]
#  t_diff  <- 'perfect year'

#  if (length(max_diff_id) == 0) {
#      plus_month <- filter(md_df, modulus == 1) 
#      max_diff_id <- which.max(plus_month$month_diff)
#      first <- plus_month$date1[max_diff_id]
#      last  <- plus_month$date2[max_diff_id]
#      t_diff <- 'plus one month'
#  } else if (length(max_diff_id) == 0) {
#      minus_month <- filter(md_df, modulus == 11)
#      max_diff_id <- which.max(minus_month$month_diff)
#      first <- minus_month$date1[max_diff_id]
#      last  <- minus_month$date2[max_diff_id]
#      t_diff <- 'minus one month'
#  }

  first_id <- which(adf$date1 == first)[1]
  last_id  <- which(adf$date2 == last)[1]

  if (is.na(first_id)) {
    first_id <- which(adf$date2 == first)[1]
  }

  if (is.na(last_id)) {
      last_id <- which(adf$date1 == last)[1]
    }
    #browser()
  if (dataset == 'richData') {
    output <- cbind(adf[first_id, c(noYCols, y1Cols)], adf[last_id, y2Cols])
  }

  if (dataset == 'cbdata') {
    timeless_cols <- dplyr::select(adf, -date1, -SppR1, -I1, -Shan1, -date2, -SppR2, 
                            -I2, -Shan2)

    output <- cbind(adf[first_id, c(names(timeless_cols), 'date1', 'SppR1', 'I1', 'Shan1')], adf[last_id, c('date2', 'SppR2', 'I2', 'Shan2')])
  }

# print notifications of what kinds of dates are being pulled for each of the 
# studies.
  if (t_diff == 'perfect year') {
    message <- 
      cat(paste0('Study: ', output$Study.ID[1], '   Site: ', output$Site, 
                 '   Taxa: ', output$taxa, '\n',
                 ' Perfect year diff -', 
                 ' first date: ', first, ' last date: ', last), 
                 '\n', '', '\n')
    message
  } else if (t_diff == 'plus one month') {
      message <- 
        cat(paste0('Study: ', output$Study.ID[1], '   Site: ', output$Site, 
                   '   Taxa: ', output$taxa, '\n',
                   ' Plus one month diff -',
                   ' first date: ', first, ' last date: ', last), 
                   '\n', '', '\n') 
      message
  } else if (t_diff == 'minus one month') {
      message <- 
        cat(paste0('Study: ', output$Study.ID[1], '   Site: ', output$Site, 
                   '   Taxa: ', output$taxa, '\n',
                   ' Minus one month diff -', 
                   ' first date: ', first, ' last date: ', last), 
                   '\n', '', '\n')
      message
  }

  return(output)

}

##############################################################################
#                           Model Summary Functions                          #
##############################################################################

mk_rma_summary_df <- function(rma_object) { 
  driver = rownames(rma_object$b)
  estimate = as.vector(rma_object$b)
  se = rma_object$se
  pval = rma_object$pval
  ci_lb = rma_object$ci.lb
  ci_ub = rma_object$ci.ub
  summ_df <- data.frame(driver = driver, estimate = estimate, se = se, 
                        pval = pval, ci_lb = ci_lb, ci_ub = ci_ub)
  summ_df <- summ_df %>% 
               mutate(sig_stars = ifelse(pval <= 0.001, '***', 
                                    ifelse(pval <= 0.01, '**', 
                                      ifelse(pval <= 0.05, '*', 
                                        ifelse(pval <= 0.1, '.', 
                                          ifelse(pval <= 1, ' '))))))
  return(summ_df)
}

mk_lme_summary_df_nk <- function(lme_object, confint_method = 'Wald') {
  driver <- rownames(coefficients(summary(lme_object)))
  estimate = round(coefficients(summary(lme_object))[, 1], digits = 3)
  se = round(coefficients(summary(lme_object))[, 2], digits = 3)
  pval = round(coefficients(summary(lme_object))[, 5], digits = 3)
  ci_lb = round(confint(lme_object, method = confint_method)[-(1:2), 1], digits = 3)
  ci_ub = round(confint(lme_object, method = confint_method)[-(1:2), 2], digits = 3)
  k = length(lme_object@frame$yi_SppR_ROM)
  n = nlevels(lme_object@frame$Study.ID)
  summ_df <- data.frame(driver = driver, estimate = estimate, se = se, 
                        pval = pval, ci_lb = ci_lb, ci_ub = ci_ub, k = k, n = n)
  summ_df <- summ_df %>% 
               mutate(sig_stars = ifelse(pval <= 0.001, '***', 
                                    ifelse(pval <= 0.01, '**', 
                                      ifelse(pval <= 0.05, '*', 
                                        ifelse(pval <= 0.1, '.', 
                                          ifelse(pval <= 1, ' '))))))
  return(summ_df)
}


##############################################################################
#                            Leave-1-out analysis                            #
##############################################################################
get_leave1out_data <- function(leave1out_results_element) {
  #browser()
  rma_object <- leave1out_results_element
  factors <- dimnames(rma_object$b)[[1]]
  b <- as.vector(rma_object$b[, 1])
  lci <- rma_object$ci.lb
  uci <- rma_object$ci.ub
  # sigma2 in metafor is the between study variation
  tau2 <- rma_object$sigma2
  # tau2 in an rma object is the within study variation
  sigma2 <- rma_object$tau2
  gamma2 <- rma_object$gamma2
  pval <-rma_object$pval 
  zval <- rma_object$zval
  rho <- rma_object$rho
  if(is.null(rho) == TRUE) {rho <- NA}
  QE <- rma_object$QE
  QEp <- rma_object$QEp
  QM <- rma_object$QM
  QMp <- rma_object$QMp
  site_count <- length(rma_object$slab)
#
  adf <- data_frame(moderator = factors, b = b, lci = lci, uci, pval = pval, 
                    zval = zval) %>% 
         mutate(QE = QE, QEp = QEp, QM = QM, QMp = QMp, tau2 = tau2, 
                sigma2 = sigma2, rho = rho, gamma2 = gamma2, 
                site_count = site_count)
  return(adf)
}

leave1out.rma.mv <- function(model_input_df, leave1out_col = c('Study.ID', 'id'), 
  model = 'rma.mv(yi = yi_SppR_ROM, V = vi_SppR_ROM, random = ~ 1 | Study.ID, mods = ~ Duration, data = leave1out_df)', samp_size_weighted = FALSE) {
  # Setup for leave1out inputs/outputs
  leave1out_df_list <- list()
  unique_studies <- unique(model_input_df[, leave1out_col])
#
  for (i in seq_along(unlist(unique_studies))) {
    filter_crit <- lazyeval::interp(~ filter_var != excluded_point, 
                                     filter_var = as.name(leave1out_col), 
                                     excluded_point = unlist(unique_studies[i, leave1out_col]))
#
  leave1out_df_list[[i]] <- 
    model_input_df %>% 
      filter_(filter_crit)
  }
  names(leave1out_df_list) <- as.character(unlist(unique_studies))
#
  # Run leave1out analysis 
  leave1out_rma_results_list <-
    lapply(leave1out_df_list, function(leave1out_df) {
      leave1out_rma_results <- eval(parse(text = model))
#      
      if (samp_size_weighted == FALSE) {
        return(leave1out_rma_results)
      } else if (samp_size_weighted == TRUE) {
          leave1out_robust <- robust(leave1out_rma_results, cluster=1:leave1out_rma_results$k)
          leave1out_robust$zval = NA
          leave1out_robust$QE = NA
          leave1out_robust$QEp = NA
          return(leave1out_robust)
        }
    })
  #browser()
  names(leave1out_rma_results_list) <- as.character(unlist(unique_studies))
#
  leave1out_rma_results_summary <- 
    lapply(leave1out_rma_results_list, get_leave1out_data)
#
  leave1out_rma_results_df <- 
    bind_rows(leave1out_rma_results_summary) %>% 
    mutate(excluded_study = rep(names(leave1out_rma_results_summary), each = nrow(leave1out_rma_results_summary[[1]])))
#
  return(leave1out_rma_results_df)
}


get_lmer_leave1out_data <- function(leave1out_results_element){
  moderators = rownames(summary(leave1out_results_element)$coefficients)
  b = summary(leave1out_results_element)$coefficients[, 1]
  lci = confint(leave1out_results_element)[-(1:2), 1]
  uci = confint(leave1out_results_element)[-(1:2), 2]
  pval = summary(leave1out_results_element)$coefficients[, 5]
#
  adf = data_frame(moderator = moderators, b = b, lci = lci, uci, pval = pval)
  return(adf)
}

leave1out.lmer <- function(model_input_df, leave1out_col = c('Study.ID', 'id'), 
  model = 'lmer(yi_SppR_ROM ~ Duration + (1 | Study.ID), data = leave1out_df)') {
  # Setup for leave1out inputs/outputs
  leave1out_df_list <- list()
  unique_studies <- unique(model_input_df[, leave1out_col])
#
  for (i in seq_along(unlist(unique_studies))) {
    filter_crit <- lazyeval::interp(~ filter_var != excluded_point, 
                                     filter_var = as.name(leave1out_col), 
                                     excluded_point = unlist(unique_studies[i, leave1out_col]))
#
  leave1out_df_list[[i]] <- 
    model_input_df %>% 
      filter_(filter_crit)
  }
  names(leave1out_df_list) <- as.character(unlist(unique_studies))
#
  # Run leave1out analysis 
  leave1out_lmer_results_list <-
    lapply(leave1out_df_list, function(leave1out_df) {
      leave1out_lmer_results <- eval(parse(text = model))
#      
        return(leave1out_lmer_results)
      })
  #browser()
  names(leave1out_lmer_results_list) <- as.character(unlist(unique_studies))
#
  leave1out_lmer_results_summary <- 
    lapply(leave1out_lmer_results_list, get_lmer_leave1out_data)
#
  leave1out_lmer_results_df <- 
    bind_rows(leave1out_lmer_results_summary) %>% 
    mutate(excluded_study = rep(names(leave1out_lmer_results_summary), each = nrow(leave1out_lmer_results_summary[[1]])))
#
  return(leave1out_lmer_results_df)
}

# Make plots for the specific driver leave one out analysis
leave1out_studies_plot <- function(single_driver_df) {
  ggplot(data = single_driver_df) + 
  geom_point(aes(x = b, y = Reference)) + 
  geom_errorbarh(aes(y = Reference, xmin = lci, xmax = uci, x = b), height = 0) +
  geom_vline(xintercept = 0, colour = 'red', linetype = 'dashed') + 
  # Make plots symmetrical
  geom_errorbarh(data = . %>% summarise(boundary = max(abs(c(lci, uci)))), aes(x = boundary / 3, xmin = -boundary / 3, xmax = boundary / 3, y = 1), alpha = 0) +
  theme_minimal() + 
  xlab('\nCoefficient estimate') + 
  ylab('Excluded study\n')
}


##############################################################################
#                              Caterpillar Plots                             #
##############################################################################
# Caterpillar plot of raw no event-weighted data points
mk_cater_plot <- function(meta_df, title, error_bar_se, weight, error_bar_alpha = 1) {
  rect_ends <- c(min(meta_df$yi_SppR_ROM - 2 * sqrt(error_bar_se)), max(meta_df$yi_SppR_ROM + 2 * sqrt(error_bar_se)))
  end <- abs(rect_ends[which.max(rect_ends)])
  cater_plot <- 
    meta_df %>% 
    arrange(Reference) %>% 
    mutate(plotting_id = 1:nrow(.), Reference = factor(Reference)) %>% 
    mutate(odd_even = ifelse(as.numeric(Reference) %% 2 == 0, 'even', 'odd')) %>% 
    ggplot(data = ., aes(x = plotting_id, y = yi_SppR_ROM, group = factor(id))) +
      geom_rect(data = . %>% filter(odd_even == 'odd') %>% group_by(Reference) %>% summarise(min_plot_id = min(plotting_id), max_plot_id = max(plotting_id)), 
        aes(x = NULL, y = NULL, xmin = min_plot_id - 0.5, xmax = max_plot_id + 0.5, ymin = -end, ymax = end, group = NULL), alpha = 0.3) +
      geom_hline(yintercept = 0, colour = 'grey70') +
      geom_errorbar(aes(ymin = yi_SppR_ROM - error_bar_se, 
                        ymax = yi_SppR_ROM + error_bar_se), 
                    width = 0, colour = 'grey40', alpha = error_bar_alpha) + 
      geom_point(aes(fill = Reference, size = weight), shape = 21, alpha = 0.7) + 
      theme_minimal() + 
      theme(axis.text = element_blank(), 
            axis.title.y = element_blank(), 
            plot.margin = unit(c(0.2, 0.2, 0.2, 4), 'cm')) + 
      xlim(0, 148) +
      guides(fill = FALSE, size = FALSE) + 
      scale_size(range = c(1.5, 10)) + 
      scale_x_continuous(expand = c(0.01, 0)) +  
      xlab('Log Ratio') + 
      coord_flip() + 
      ggtitle(title)
#
  ref_labs <- 
    meta_df %>% 
      arrange(Reference) %>% 
      mutate(plotting_id = 1:nrow(.), Reference = factor(Reference)) %>% 
      group_by(Reference) %>% 
      summarise(mean_plotting_id = mean(plotting_id))
#
  for (i in 1:nrow(ref_labs))  {
    cater_plot <- cater_plot + annotation_custom(
        grob = textGrob(label = ref_labs$Reference[i], hjust = 1.01, vjust = 0.5, gp = gpar(cex = 0.8)),
        ymin = -end,    # Vertical position of the textGrob
        ymax = -end,
        xmin = ref_labs$mean_plotting_id[i],  # Note: The grobs are positioned outside the plot area
        xmax = ref_labs$mean_plotting_id[i]
        )
   }
  return(cater_plot)
}

draw_cater_plot <- function(cater_plot) {
  # Code to override clipping
  grid.newpage()
  gt <- ggplot_gtable(ggplot_build(cater_plot))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
}
