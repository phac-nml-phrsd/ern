#' @return List with components
#' - `dist` distribution type (_e.g._ `lnorm` = log-normal, `gamma` = Gamma, `unif` = uniform)
#' - `mean` distribution mean (only for `dist` = `lnorm` or `gamma`)
#' - `mean_sd` standard deviation of the mean (only for `dist` = `lnorm` or `gamma`)
#' - `sd` standard deviation (only for `dist` = `lnorm` or `gamma`)
#' - `sd_sd` standard deviation of the standard deviation (only for `dist` = `lnorm` or `gamma`)
#' - `min` minimum value of the random variable modelled by this distribution (only for `dist` = `unif`)
#' - `max` maximum value of the random variable modelled by this distribution
