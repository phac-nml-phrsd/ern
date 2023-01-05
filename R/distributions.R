#' Define the incubation period distribution
#'
#' @template return-dist
#' @export
def_dist_incubation_period <- function(){
  list(
    dist = "lnorm",
    mean = 1.621,
    mean_sd = 0.064,
    sd = 0.418,
    sd_sd = 0.0691,
    max = 15
  )
  # copied over from EpiNow2
  # EpiNow2::get_incubation_period(
  #   disease = "SARS-CoV-2",
  #   source = "lauer"
  # )
}

#' Define the generation interval distribution
#'
#' @template return-dist
#' @export
def_dist_generation_interval <- function(){
  list(
    dist = "gamma",
    mean = 3.635272,
    mean_sd = 0.7109351,
    sd = 3.07531,
    sd_sd = 0.7695178,
    max = 15
  )
  # copied over from EpiNow2
  # EpiNow2::get_generation_time(
  #   disease = "SARS-CoV-2",
  #   source = "ganyani"
  # )
}

#' Define the reporting fraction distribution
#'
#' @template return-dist
#' @export
def_dist_reporting_fraction <- function(){
  list(
      dist = "unif",
      min = 0.1,
      max = 0.3
  )
  # just a guess
}

#' Sample parameters for a single distribution assuming parameters
#' come from a truncated normal
#'
#' @param dist a list of distribution parameters, as defined by the `def_dist_*()` functions
sample_a_dist <- function(dist){

  # get bounds for truncated normals
  mean_bounds <- get_tnorm_bounds(dist$mean, dist$mean_sd)
  sd_bounds <- get_tnorm_bounds(dist$sd, dist$sd_sd)

  # sample parameters
  mean = TruncatedNormal::rtnorm(n = 1,
                mu = dist$mean,
                lb = mean_bounds[1],
                ub = mean_bounds[2])
  sd = TruncatedNormal::rtnorm(n = 1,
              mu = dist$sd,
              lb = sd_bounds[1],
              ub = sd_bounds[2])

  # return in standardized distribution format
  list(
    dist = dist$dist,
    mean = mean,
    mean_sd = NA,
    sd = sd,
    sd_sd = NA,
    max = dist$max
  )
}

#' Get a discretized, truncated version of a distribution
#'
#' @param params distribution params (output of `def_dist_*()` function)
#'
#' @return vector with discretized density
#' @export
#'
#' @examples `prm <- def_dist_incubation_period(); get_discrete_dist(prm)`
get_discrete_dist <- function(params){
  if(!(params$dist %in% c("lnorm", "gamma"))) stop("distribution recipe has not been defined")

  if(params$dist == "lnorm"){
    x <- stats::dlnorm(
      1:params$max,
      meanlog = params$mean,
      sdlog = params$sd
    )
  }

  if(params$dist == "gamma"){
    x <- stats::dgamma(
      1:params$max,
      shape = params$mean^2/params$sd^2,
      scale = params$sd^2/params$mean
    )
  }

  # normalize to 1
  x/sum(x)
}

#' Get non-negative truncated normal bounds
#'
#' @param mean mean
#' @param sd standard deviation
get_tnorm_bounds <- function(mean, sd){
  # figure out if mean-2*sd is below zero; if it is,
  # take 0 as the lower bound, and set the
  # upper bound as 2*mean (for symmetry about mean)
  lb = max(0, mean - 2*sd)
  ub = ifelse(lb == 0, 2*mean,
              mean + 2*sd)

  # return
  c(lb, ub)
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
