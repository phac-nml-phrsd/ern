
#' @title Build aggregation matrix A
#' 
#' @param N Integer. Total number of days after disaggregation.
#' @param obs_times Integer vector. Observation times of the aggregated (reported) incidence.
#' @param window Integer. Aggregation window size. For example, \code{window = 7} for weekly aggregation. 
#' @keywords internal
#' 
build_A <- function(N, obs_times, window) {
  K <- length(obs_times)
  
  A <- Matrix::Matrix(0, nrow = K, ncol = N, sparse = TRUE)
  
  for (k in seq_len(K)) {
    t <- obs_times[k]
    idx <- (t - window):(t - 1)
    
    # Keep only valid indices
    idx <- idx[idx >= 1 & idx <= N]
    stopifnot(length(idx)>0)
    A[k, idx] <- 1
  }
  return(A)
}

#' @title Build second-difference matrix D, that is:
#' D y = y_{t+1} - 2 y_t + y_{t-1}
#' 
#' @param N Integer. Total number of days after disaggregation.
#' @keywords internal
#' 
build_D2 <- function(N) {
  D <- Matrix::Matrix(0, 
                      nrow = N - 2, 
                      ncol = N, 
                      sparse = TRUE)
  
  for (i in 1:(N - 2)) {
    D[i, i]     <- 1
    D[i, i + 1] <- -2
    D[i, i + 2] <- 1
  }
  return(D)
}

# @description Solver to disaggregate weekly reported incidence into daily.
#' Title
#'
#' @param z Numerical vector. Aggregated (reported) incidence.
#' @param obs_times Integer vector. Observation times of the aggregated (reported) incidence.
#' @param N Integer. Total number of days after disaggregation.
#'
#' @returns Numerical vector of daily disaggregated incidence.
#'
#' @keywords internal
#'
solve_temporal_disagg <- function(z, obs_times, N, window) {
  
  A <- build_A(N = N, obs_times = obs_times, window = window)
  D <- build_D2(N)
  
  # convert to base matrices
  A <- as.matrix(A)
  D <- as.matrix(D)
  
  y <- CVXR::Variable(N)
  
  # stable quadratic form
  Q <- t(D) %*% D
  
  objective <- CVXR::Minimize( CVXR::quad_form(y, Q) )
  
  constraints <- list(
    A %*% y == z,
    y >= 0
  )
  
  problem = CVXR::Problem(objective, constraints)
  result  = solve(problem, solver = "OSQP")
  
  st = CVXR::status(problem)
  if (st != "optimal") {
    warning(paste("Solver status:", st))
  }
  
  out = as.numeric(CVXR::value(y))
  return(out)
}

#' Disaggregate reported incidence (typically weekly)
#' into daily incidence. "Smooth" method.
#'
#' @param cl.data Dataframe of aggregated incidence. 
#' Must have columns named \code{t} and \code{value}.
#' @param smooth.input Logical. Should the _input_ aggregated incidence
#' be smoothed before being processed? If TRUE, will perform a
#' LOESS smoothing. 
#' @param smooth.input.span Numerical. "span" parameter for the \code{loess()} 
#' smoothing function.  
#'
#' @returns Dataframe of inferred daily incidence.
#' @keywords internal
#'
smooth_disaggregation_daily <- function(cl.data, 
                                        smooth.input,
                                        smooth.input.span) {
  z = cl.data$value
  obs_times = cl.data$t
  
  # Smooth aggregated incidence
  zs = z
  if(smooth.input){
    q = loess(data.frame(t = obs_times, z = z),
              formula = z ~ t, 
              span = smooth.input.span)
    zs = q$fitted
    zs[zs < 0] = 0
    
    if(0){plot(z) ; lines(zs)} # DEBUG
  }
  
  N       = max(cl.data$t)
  dt      = diff(obs_times)
  
  # Retrieve the most frequent reporting frequency
  # (should typically be 7 for weekly reports)
  window = as.numeric(names(sort(table(dt), decreasing = TRUE))[1])
  
  y = solve_temporal_disagg(
    z = zs, 
    obs_times = obs_times,
    N = N, 
    window = window)
  
  # The constraints on the minimization problem
  # should guarantee y>=0 but some may have 
  # tiny negative values (numerical error?),
  # so positivity is forced:
  y[y < 0] = 0
  
  # Build a dataframe formatted for downstream processing
  date.first = cl.data$date[1] - cl.data$t[1] + 1 
  inc.daily = data.frame(
    id    = 1L, 
    t     = 1:N,
    date  = date.first + 1:N - 1,
    value = y
  ) 
  return(inc.daily)
}

