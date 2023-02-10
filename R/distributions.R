#' Define the incubation period distribution
#'
#' @template return-dist
#' @export
def_dist_incubation_period <- function(){
  list(
    dist = "gamma",
    mean = 3.49,
    mean_sd = 0.1477,
    shape = 8.5,
    shape_sd = 1.8945,
    max = 8
  )
  # see docs/distribution-params.html for refs
}

#' Define the generation interval distribution
#'
#' @template return-dist
#' @export
def_dist_generation_interval <- function(pathogen = 'sars-cov-2'){
  
  x = NULL
  
  p = tolower(pathogen)
  
  if(p == 'sars-cov-2'){
    x <- list(
      dist = "gamma",
      mean = 6.84,
      mean_sd = 0.7486,
      shape = 2.39,
      shape_sd = 0.3573,
      max = 15)
  }
  
    if(p == 'influenza'){
    x <- list(
      dist = "gamma",
      mean = 5.5,
      mean_sd = 1.1,
      shape = 2,
      shape_sd = 0.3,
      max = 14)
  }
  
  return(x)
  # see docs/distribution-params.html for refs
}

#' Define the reporting fraction distribution
#'
#' @template return-dist
def_dist_reporting_fraction <- function(){
  list(
      dist = "unif",
      min = 0.1,
      max = 0.3
  )
  # just a guess
}


#' Draw from gamma based on parameter name in dist list
#'
#' @param par String. Name of the parameter to sample.
#' @param dist List. Distribution definition.
#'
#' @return Numeric. The sampled value.
#'
draw_from_gamma <- function(par, dist){
  mean = dist[[par]]
  sd_2 = (dist[[paste0(par, "_sd")]])^2

  shape = mean^2 / sd_2
  scale = sd_2 / mean

  return( stats::rgamma(n = 1, shape = shape, scale = scale) )
}


#' Sample parameters for a single distribution assuming parameters
#' come from a Gamma distribution
#'
#' @param dist a list of distribution parameters, as defined by the `def_dist_*()` functions
sample_a_dist <- function(dist){

  # get parameter names that we're sampling
  par_names <- gsub("_sd", "", grep("_sd$", names(dist), value = TRUE))

  draw <- lapply(par_names, draw_from_gamma, dist = dist)

  # == DEBUG
  # print(paste('DEBUG sad:', draw))
  # ==

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
#' @return vector with discretized density
get_discrete_dist <- function(params){

  # check args
  # -------------------------
  check_dist(params)

  if(!(params$dist %in% c("lnorm", "gamma"))) stop(paste0("Distribution recipe has not
been defined for specified distribution type (dist = ", params$dist, ")"))

  # get discrete dist
  # -------------------------
  if(params$dist == "lnorm"){
    x <- stats::dlnorm(
      1:params$max,
      meanlog = params$mean,
      sdlog = params$sd
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

  # normalize to 1 and return
  # -------------------------
  x/sum(x)
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
