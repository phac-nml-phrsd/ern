# - - - - - - - - - - - - - - - - -
# Distribution definitions
# - - - - - - - - - - - - - - - - -

#' @title Define a family of incubation period distributions
#'
#' @template param-dist
#' @template return-dist
#' @export
#' 
#' @examples 
#' d = def_dist_incubation_period(
#'   dist     = "gamma",
#'   mean     = 3.49,
#'   mean_sd  = 0.1477,
#'   shape    = 8.5,
#'   shape_sd = 1.8945,
#'   max      = 8
#' )
#' print(d)
#' 
def_dist_incubation_period <- function(dist, ...){
  return(
    list(
      dist,
      ...
    )
  )
}

#' @title Define a family of generation interval distributions
#'
#' @template param-dist
#' @template return-dist
#' @export
#' 
#' @examples 
#' d = def_dist_generation_interval(
#'   dist     = "gamma",
#'   mean     = 6.84,
#'   mean_sd  = 0.7486,
#'   shape    = 2.39,
#'   shape_sd = 0.3573,
#'   max      = 15
#' )
#' print(d)
#' 
def_dist_generation_interval <- function(dist, ...){
  return(
    list(
      dist,
      ...
    )
  )
}

#' Define a reporting fraction distribution
#'
#' @template param-dist
#' @template return-dist
#' @export
#' 
#' @examples 
#' d = def_dist_reporting_fraction(
#'   dist = "unif",
#'   min = 0.1,
#'   max = 0.3
#' )
#' print(d)
#' 
#' 
def_dist_reporting_fraction <- function(dist, ...){
  return(
    list(
      dist,
      ...
    )
  )
}

#' @title Define a family of fecal shedding distributions
#'
#' @template param-dist
#' @template return-dist
#' @export
#'
#' @examples
#' fec = def_dist_fecal_shedding(
#'   dist = "gamma",
#'   mean = 12.90215,
#'   mean_sd = 1.136829,
#'   shape = 1.759937,
#'   shape_sd = 0.2665988,
#'   max = 33
#' )
#' print(fec)
#'
def_dist_fecal_shedding <- function(dist, ...) {
  return(
    list(
      dist,
      ...
    )
  )
}

#' Define a family of reporting delay distributions
#'
#' Default values come from analyzing COVID-19 line list data across all of Canada for the time period matching the sample COVID-19 clinical data provided in [ern::cl.input]
#'
#' @template param-dist
#' @template return-dist
#'
#' @export
#' 
#' @examples 
#' d = def_dist_reporting_delay(
#'   dist = 'gamma',
#'   mean = 6.99,
#'   mean_sd = 0.2211,
#'   sd = 3.663,
#'   sd_sd = 0.1158,
#'   max = 21
#' )
#' print(d)
#' 
def_dist_reporting_delay <- function(dist, ...){
  return(
    list(
      dist,
      ...
    )
  )
}

# - - - - - - - - - - - - - - - - -
# Distribution utilities
# - - - - - - - - - - - - - - - - -

#' @title Draw from gamma for a parameter specified in a distribution family list.
#'
#' @param par Character. Name of the parameter to sample.
#' @param dist List. Distribution definition, as output by a `def_dist_*()` function.
#' @keywords internal
#' @return Numeric. The sampled parameter value.
#'
draw_from_gamma <- function(par, dist){
  mean = dist[[par]]
  sd_2 = (dist[[paste0(par, "_sd")]])^2

  shape = mean^2 / sd_2
  scale = sd_2 / mean

  res = stats::rgamma(n = 1, shape = shape, scale = scale)
  return( res )
}

#' @title Sample parameters for a single distribution from a family of 
#' distributions, assuming parameters come from a Gamma distribution.
#
#' @keywords internal
#' @param dist List. A list of distribution parameters, as defined by the
#'  `def_dist_*()` functions.
#'
sample_a_dist <- function(dist){

  # get parameter names that we're sampling
  par_names <- gsub("_sd", "", grep("_sd$", names(dist), value = TRUE))

  draw <- lapply(par_names, draw_from_gamma, dist = dist)

  # === convert to standardized distribution format
  #
  empty <- rep(NA, length(draw))
  out <- unlist(
    lapply(X = 1:length(par_names),
           FUN = function(i){
             list(draw[[i]], empty[[i]])
           }),
    recursive = FALSE)

  out_names <- unlist(lapply(par_names,
                             function(x) paste0(x, c("", "_sd"))))
  names(out) <- out_names

  # Check that the std dev is not too small.
  # An extremely small std dev :
  #   - is not in the spirit of a genuinely _random_ parameter
  #   - has the `dgamma`, `dnorm`, etc. to throw all zeros densities
  #     which breaks the program as we divide by sum(densities)
  #     to normalize the density function to 1 (to have a probability)
  
  # To scale the minimum value to the mean of the parameter,
  # we impose a minimum value to the coefficient of variation:
  
  if('sd' %in% names(out)){
    cv.min = 0.01
    sd.min = out[['mean']] * cv.min
    if(out[['sd']] < sd.min) {
      out[['sd']] <- sd.min
    }
  }
  
  # return final list
  res = c(
    list(dist = dist$dist),
    out,
    list(max = dist$max) )

  return(res)
}

#' Get a discretized, truncated version of a distribution
#'
#' @param params distribution params (output of `def_dist_*()` function)
#'
#' @return Numeric. Vector with discretized density.
#' @export
#' 
#' @examples 
#' 
#' # Define distributions
#' fec = def_dist_fecal_shedding(pathogen = 'sarscov2')
#' gi  = def_dist_generation_interval(pathogen = 'influenza')
#' 
#' # Get their (discretized) densities
#' d.fec = get_discrete_dist(fec)
#' d.gi  = get_discrete_dist(gi)
#' 
#' print(d.fec)
#' print(d.gi)
#' 
get_discrete_dist <- function(params){

  # --- check args
  check_dist(params)

  if(!(params$dist %in% c("lnorm", "gamma", "norm"))) {
    stop(paste0("Distribution recipe has not been defined
                for specified distribution type (dist = `",
                params$dist, "`)"))
  }

  # --- get discrete dist

  if(params$dist == "lnorm"){
    x <- stats::dlnorm(
      x       = 1:params$max,
      meanlog = params$meanlog,
      sdlog   = params$sdlog
    )
  }

  if(params$dist == "norm"){
    x <- stats::dnorm(
      x    = 1:params$max,
      mean = params$mean,
      sd   = params$sd
    )
  }

  if(params$dist == "gamma"){
    if("sd" %in% names(params)){
      shape = params$mean^2/params$sd^2
      scale = params$sd^2/params$mean
    } else if("shape" %in% names(params)){
      shape = params$shape
      scale = params$mean/shape
    }
    x <- stats::dgamma(
      1:params$max,
      shape = shape,
      scale = scale
    )
  }

  # --- normalize to 1 and return
  return(x/sum(x))
}

#' @title Sample from a distribution 
#' (currently only implemented for a uniform distribution)
#' @keywords internal
#' @param n number of samples to draw
#' @param params distribution parameters
sample_from_dist <- function(n, params){
  if(!(params$dist %in% c("unif"))) stop("distribution recipe has not been defined")

  if(params$dist == "unif"){
    return(stats::runif(n = n, min = params$min, max = params$max))
  }
}



