#Meta-analysis functions

###########################################################################
#                        Make summary data frames                         #
###########################################################################

# Make a summary dataframe using an rma object so that you can get 
# mods = "string of mods column name"
# Need to fix when certain mods are dropped because the number of points is too
# small, write better num_studies and num_sites calculation)
mk_summary_df <- function(original_df, mods, rma_object, moderators = FALSE) {
    factors <- dimnames(rma_object$b)[[1]]
    mean_estimate <- as.data.frame(rma_object$b)
    lower_ci <- rma_object$ci.lb
    upper_ci <- rma_object$ci.ub
    # sigma2 in metafor is the between study variation
    tau2 <- rma_object$sigma2
    # tau2 in an rma object is the within study variation
    sigma2 <- rma_object$tau2
    rho <- rma_object$rho
    if(is.null(rho) == TRUE) {rho <- NA}
    if (moderators == FALSE) {
        num_studies <- length(unique(original_df$Study.ID))
        num_sites <- length(unique(original_df$Site))
        df <- data.frame(mean_estimate = mean_estimate, lower_ci = lower_ci,
                         upper_ci, sigma2 = sigma2, rho = rho, 
                         studies_per_mod = num_studies, 
                         sites_per_mod = num_sites)
        colnames(df) <- c("mean_estimate", "lower_ci", "upper_ci", 
                          "sigma2", "rho", "studies_per_mod", "sites_per_mod")
    } else {
        num_studies <- ddply(original_df, mods, summarise, 
                             studies = length(unique(Study.ID))
                             )
        num_sites <- ddply(original_df, mods, summarise, 
                           studies = length(unique(Site))
                           )
        df <- data.frame(moderator = factors, mean_estimate = mean_estimate, 
                         lower_ci = lower_ci, upper_ci, sigma2 = sigma2, rho = rho, 
                         studies_per_mod = num_studies[, 2], sites_per_mod = num_sites[, 2])
        colnames(df) <- c("moderator", "mean_estimate", "lower_ci", "upper_ci", 
                          "sigma2", "rho", "studies_per_mod", "sites_per_mod")
    }
    return(df)
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

# Select the first and last data points from the largest time difference 
# (+/- one month)
get_month_diff <- function(date1, date2) {
    md <- (year(date2) - year(date1)) * 12  + abs(month(date2) - month(date1))
    return(md)
}

get_first_last <- function(adf) {
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

  perfect_year <- filter(md_df, modulus == 0)
  max_diff_id <- which.max(perfect_year$month_diff)
  first <- perfect_year$date1[max_diff_id]
  last  <- perfect_year$date2[max_diff_id]
  t_diff  <- 'perfect year'

  if (length(max_diff_id) == 0) {
      plus_month <- filter(md_df, modulus == 1) 
      max_diff_id <- which.max(plus_month$month_diff)
      first <- plus_month$date1[max_diff_id]
      last  <- plus_month$date2[max_diff_id]
      t_diff <- 'plus one month'
  } else if (length(max_diff_id) == 0) {
      minus_month <- filter(md_df, modulus == 11)
      max_diff_id <- which.max(minus_month$month_diff)
      first <- minus_month$date1[max_diff_id]
      last  <- minus_month$date2[max_diff_id]
      t_diff <- 'minus one month'
  }

  first_id <- which(adf$date1 == first)[1]
  last_id  <- which(adf$date2 == last)[1]

  if (is.na(first_id)) {
    first_id <- which(adf$date2 == first)[1]
  }

  if (is.na(last_id)) {
      last_id <- which(adf$date1 == last)[1]
    }

  output <- cbind(adf[first_id, c(noYCols, y1Cols)], adf[last_id, y2Cols])

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