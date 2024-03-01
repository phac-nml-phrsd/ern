
#' @title Daily incidence from linear interpolation 
#'
#' @param cl.data Aggregated incidence.
#'
#' @return A dataframe of daily incidence
#'

linear_int_daily <- function(cl.data) {
  
  df    = cl.data
  tau   = df$t
  A     = df$value
  delta = diff(tau)
  tmax  = max(tau)
  y     = numeric(tmax)
  
  # --- CASE k = 0
  
  m = 2 * A[1] / tau[1] / (tau[1]+1)
  y[1:tau[1]] = m * 1:tau[1]

  # --- CASE k >= 1
  
  for(k in 1:(length(A)-1)){
    m = 2 * (A[k+1] - delta[k] * y[tau[k]]) / delta[k] / (delta[k]+1)
    idx = 1:(tau[k+1]-tau[k])
    y[tau[k] + idx] = pmax(0, y[tau[k]] + m * idx)
    # message(k, " DEBUG: sum_y = ", round(sum(y[tau[k] + idx]),2), "\t; A = ", A[k+1])
  }
  
  # Result
  date.first = df$date[1] - df$t[1] + 1
  
  inc.daily = data.frame(
    id    = 1L, 
    t     = 1:tmax,
    date  = date.first + 1:tmax - 1,
    value = y
  )
  return(inc.daily)  
}


# Tue Feb 13 17:39:36 2024 ------------------------------
# STOPPED HERE
# IT SEEMS THERE IS A ONE-WEEK LAG WHEN PLOTTING
# `plot_diagnostic_cl()`
# ==> investigate!




