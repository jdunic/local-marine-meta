#Meta-analysis functions

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

# Make a summary dataframe using an rma object so that you can get 
mk_summary_df <- function(original_df, mods, rma_object) {
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
    num_studies <- ddply(original_df, .(mods), summarise, 
                         studies = length(unique(Study.ID))
                         )
    num_sites <- ddply(original_df, .(mods), summarise, 
                       studies = length(unique(Site))
                       )
    df <- data.frame(moderator = factors, mean_estimate = mean_estimate, 
                     lower_ci = lower_ci, upper_ci, sigma2 = sigma2, rho = rho, 
                     studies_per_mod = num_studies[, 2], sites_per_mod = num_sites[, 2])
    colnames(df) <- c("moderator", "mean_estimate", "lower_ci", "upper_ci", 
                      "sigma2", "rho", "studies_per_mod", "sites_per_mod")
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

