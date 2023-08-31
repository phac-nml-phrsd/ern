# - - - - - - - - - - - - - - - - -
# Distribution definitions
# - - - - - - - - - - - - - - - - -

#' Define a family of incubation period distributions
#'
#' @template param-pathogen
#' @template return-dist
#' @export
def_dist_incubation_period <- function(pathogen = 'sarscov2'){

  res = NULL
  p = tolower(pathogen)

  if(p %in% c('sarscov2', 'cov2', 'sars2')){
    res = list(
      dist     = "gamma",
      mean     = 3.49,
      mean_sd  = 0.1477,
      shape    = 8.5,
      shape_sd = 1.8945,
      max      = 8
    )
    # see docs/distribution-params.html for refs
  }

  if(p %in% c('influenza', 'flua', 'flub')){
    # reference: Lessler et al. The Lancet Infectious Diseases. 2009.
    # https://doi.org/10.1016/S1473-3099(09)70069-6
    res = list(
      dist     = "gamma",
      mean     = 2.5,
      mean_sd  = 0.5,
      sd       = 1.0,
      sd_sd    = 0.2,
      max      = 6
    )
  }

  if(p %in% c('rsv', 'rsva', 'rsvb')){
    # reference: Lessler et al. The Lancet Infectious Diseases. 2009.
    # https://doi.org/10.1016/S1473-3099(09)70069-6
    res = list(
      dist     = "gamma",
      mean     = 5.0,
      mean_sd  = 1.0,
      sd       = 1.0,
      sd_sd    = 0.2,
      max      = 6
    )
  }
  if(is.null(res)){
    stop(paste0("Unkown incubation period distribution for pathogen `", pathogen,"` not found. Aborting!"))
  }
  return(res)
}

#' Define a family of generation interval distributions
#'
#' @template param-pathogen
#' @template return-dist
#' @export
def_dist_generation_interval <- function(pathogen = 'sarscov2'){

  x = NULL
  p = tolower(pathogen)

  if(p %in% c('sarscov2', 'cov2', 'sars2')){
    # see docs/distribution-params.html for refs
    x <- list(
      dist     = "gamma",
      mean     = 6.84,
      mean_sd  = 0.7486,
      shape    = 2.39,
      shape_sd = 0.3573,
      max      = 15)
  }

  if(p %in% c('influenza', 'flua', 'flub')){
    # ref: te Beest et al. Epidemiology. 2013
    # https://www.jstor.org/stable/23487826
    x <- list(
      dist     = "gamma",
      mean     = 2.5,
      mean_sd  = 0.5,
      sd       = 1.0,
      sd_sd    = 0.3,
      max      = 8)
  }

  if(p %in% c('rsv', 'rsva', 'rsvb')){
    # ref: Otomaru et al. Open Forum Infectious Diseases. 2019.
    # https://doi.org/10.1093/ofid/ofz045
    # TODO: explore more the literature...
    x <- list(
      dist     = "gamma",
      mean     = 3.2,
      mean_sd  = 0.5,
      sd       = 1.1,
      sd_sd    = 0.1,
      max      = 8)
  }

  # Check if pathogen was found
  if(is.null(x)){
    stop(paste0("Unkown generation interval distribution for pathogen `", pathogen,"` not found. Aborting!"))
  }

  return(x)
}

#' Define a reporting fraction distribution
#'
#' @template return-dist
#' @export
def_dist_reporting_fraction <- function(){

  if(TRUE){
    res = list(
      dist = "unif",
      min = 0.1,
      max = 0.3
    )
  }

  warning('def_dist_reporting_fraction is NOT defined. Using Unif(0.1,0.3) by defaut!')

  return(res)
}

#' @title Define a family of fecal shedding distributions
#'
#' @template param-pathogen
#' @param subtype Character. Pathogen sub-type label.
#'
#' @template return-dist
#' @export
#'
#' @examples
#' fec = def_dist_fecal_shedding('sarscov2')
#' print(fec)
#'
def_dist_fecal_shedding <- function(pathogen = 'sarscov2', subtype = '') {

  p   = tolower(pathogen)

  # Check if correct pathogen specified
  if(!isTRUE(p %in% c('sarscov2', 'influenza', 'rsv'))){
    stop("Pathogen not found. Aborting!")
  }

  fec = NULL

  if(p == 'sarscov2'){
    # ref: Nourbakhsh et. al. (Epidemics, 2021)
    # https://doi.org/10.1016/j.epidem.2022.100560

    fec = list(
      dist = "gamma",
      mean = 12.90215,
      mean_sd = 1.136829,
      shape = 1.759937,
      shape_sd = 0.2665988,
      max = 33
    )

    if(subtype == 'foo'){
      # TO DO...
    }
  }


  if(p == 'influenza'){
    # This is a dummy distribution
    # TODO: look at the literature to inform those values
    fec = list(
      dist    = "gamma",
      mean    = 9,
      mean_sd = 1,
      sd      = 6,
      sd_sd   = 1,
      max     = 29
    )
    if(subtype == 'foo'){
      # TO DO...
    }
  }

  if(p == 'rsv'){
    # This is a dummy distribution
    # TODO: look at the literature to inform those values
    fec = list(
      dist    = "gamma",
      mean    = 12,
      mean_sd = 3,
      sd      = 7,
      sd_sd   = 2,
      max     = 29
    )
    if(subtype == 'foo'){
      # TO DO...
    }
  }

  return(fec)
}

#' Define a family of reporting delay distributions
#'
#' @template return-dist
#' @template param-pathogen
#'
#' @export
def_dist_reporting_delay <- function(pathogen = 'sarscov2'){
  if(!(pathogen %in% c('sarscov2'))){
    stop(paste0("No default reporting delay distribution for pathogen `", pathogen,"`. Aborting!"))
  }

  if(pathogen == 'sarscov2'){
    # TODO: this is just a dummy distribution, get real thing from linelist
    list(
      dist = 'gamma',
      mean = 5,
      mean_sd = 1,
      sd = 1,
      sd_sd = 0.1,
      max = 10
    )
  }
}

# - - - - - - - - - - - - - - - - -
# Distribution utilities
# - - - - - - - - - - - - - - - - -

#' Draw from gamma for a parameter specified in a distribution family list.
#'
#' @param par Character. Name of the parameter to sample.
#' @param dist List. Distribution definition, as output by a `def_dist_*()` function.
#'
#' @return Numeric. The sampled parameter value.
#'
draw_from_gamma <- function(par, dist){
  mean = dist[[par]]
  sd_2 = (dist[[paste0(par, "_sd")]])^2

  shape = mean^2 / sd_2
  scale = sd_2 / mean

  return( stats::rgamma(n = 1, shape = shape, scale = scale) )
}

#' Sample parameters for a single distribution from
#' a family of distributions, assuming parameters come from a Gamma distribution
#'
#' @param dist List. A list of distribution parameters, as defined by the
#'  `def_dist_*()` functions.
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

#' Sample from a distribution
#' (currently only implemented for a uniform distribution)
#'
#' @param n number of samples to draw
#' @param params distribution parameters
sample_from_dist <- function(n, params){
  if(!(params$dist %in% c("unif"))) stop("distribution recipe has not been defined")

  if(params$dist == "unif"){
    return(stats::runif(n = n, min = params$min, max = params$max))
  }
}



