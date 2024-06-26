% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_R_cl.R
\name{estimate_R_cl}
\alias{estimate_R_cl}
\title{Estimate the effective reproduction from clinical report data}
\usage{
estimate_R_cl(
  cl.data,
  dist.repdelay,
  dist.repfrac,
  dist.incub,
  dist.gi,
  prm.daily = list(method = "linear", popsize = NULL, burn = 500, iter = 2000, chains =
    3, prior_R0_shape = 2, prior_R0_rate = 0.6, prior_alpha_shape = 1, prior_alpha_rate =
    1, first.agg.period = NULL),
  prm.daily.check = list(agg.reldiff.tol = 10),
  prm.smooth = list(method = "rollmean", align = "right", window = 7),
  prm.R = list(iter = 10, CI = 0.95, window = 7, config.EpiEstim = NULL),
  RL.max.iter = 10,
  silent = FALSE
)
}
\arguments{
\item{cl.data}{Data frame. Must have variables:
\itemize{
\item \code{date}: calendar date of report
\item \code{value}: count of reported cases
}}

\item{dist.repdelay}{List. Parameters for the reporting delay distribution in the same format as returned by \code{\link[=def_dist]{def_dist()}}.}

\item{dist.repfrac}{List. Parameters for the reporting fraction distribution in the same format as returned by \code{\link[=def_dist]{def_dist()}}.}

\item{dist.incub}{List. Parameters for the incubation period distribution in the same format as returned by \code{\link[=def_dist]{def_dist()}}.}

\item{dist.gi}{List. Parameters for the generation interval distribution in the same format as returned by \code{\link[=def_dist]{def_dist()}}.}

\item{prm.daily}{List. Parameters for daily report inference via MCMC. Elements include:
\itemize{
\item \code{method}: String. Method name to infer the daily incidence reports from aggregated ones.
Either \code{linear} or \code{renewal} is currently implemented.
The \code{linear} method simply performs a linear interpolation that matches the aggregated values.
The \code{renewal} method fits a SIR-like model using a renewal equation to infer the daily incidence.
In this case, the fitting algorithm is a Markov Chain Monte Carlo (MCMC) implemented in JAGS
and needs the parameters below (e.g., \code{burn,iter,chains,...}).
The \code{renewal} method is more adapted for short single wave epidemics as this models
i) naturally fits a single wave and ii) has longer computing time.
For longer time series, user may perfer the \code{linear} method.
\item \code{popsize}: Integer. Population size to use in MCMC simulation to infer daily observations from aggregated input data.
\item \code{burn}: Numeric. Length of burn-in period (number of days).
\item \code{iter}: Numeric. Number of iterations after burn-in period (number of days).
\item \code{chains}: Numeric. Number of chains to simulate.
\item \code{prior_R0_shape}: Shape of the (hyper-)parameter for the prior Gamma distribution for R0.
\item \code{prior_R0_rate}: Rate of the (hyper-)parameter for the prior Gamma distribution for R0.
\item \code{prior_alpha_shape}: Shape of the (hyper-)parameter for the prior Gamma distribution for alpha.
\item \code{prior_alpha_rate}: Rate of the (hyper-)parameter for the prior Gamma distribution for alpha.
\item \code{first.agg.period}: length of aggregation period for first aggregated observation (number of days); if NULL, assume same aggregation period as observed for second observation (gap between first and second observations)
}}

\item{prm.daily.check}{List. Parameters for checking aggregated to daily report inference. Elements include:
\itemize{
\item \code{agg.reldiff.tol}: numerical tolerance (\%) for relative error between aggregated inferred daily reports and original aggregated reports; chronological observations are dropped until this tolerance is first acheived (convergence at the start of the timeseries is often the worst, need to maintain uninterrupted daily timeseries for input into Rt calculation).
}
Set this entire argument to \code{NULL} to use inferred daily reports as is.}

\item{prm.smooth}{List. list of smoothing parameters. Parameters should be
specified as followed:
\itemize{
\item \code{method}: smoothing method, either \code{'rollmean'} (rolling mean) or \code{'loess'} (LOESS smoothing via \code{\link[stats:loess]{stats::loess()}})
\item \code{window}: for \verb{method = 'rollmean} only; width of smoothing window in days
\item \code{align}: for \verb{method = 'rollmean} only; smoothing alignment, either \code{'center'}, \code{'left'}, \code{'right'}
\item \code{span}: for \code{method = 'loess'} only; smoothing span (see the documentation for \code{\link[stats:loess]{stats::loess()}} for details)
\item \code{floor}: optional call for wastewater concentration smoothing with \code{method = 'loess'} only; user defined minimum smoothing concentration
}
Set this entire list to \code{NULL} to turn off smoothing}

\item{prm.R}{List. Settings for the ensemble when calculating Rt.
Elements include:
\itemize{
\item \code{iter}: Integer. Number of iterations for the Rt ensemble
\item \code{CI}: Numeric between 0 and 1. Confidence interval width for Rt
estimates after sampling uncertain distributions.
\item \code{window}: Integer. Number of days defining the window of data used by
\code{EpiEstim} to estimate Rt. If \code{NULL}, will default to 7.
\item \code{config.EpiEstim}: (optional) configuration for \code{EpiEstim} defined via
\code{\link[EpiEstim:make_config]{EpiEstim::make_config()}}. If \code{NULL}, will use default config from
\code{EpiEstim}.
}}

\item{RL.max.iter}{Integer. Maximum of iterations for the Richardson-Lucy deconvolution algorithm.}

\item{silent}{Logical. Flag to suppress all output messages, warnings, and progress bars.}
}
\value{
List. Elements include:
\itemize{
\item \code{cl.data}: original aggregated reports signal
\item \code{cl.daily}: reports as input for Rt calculation (inferred daily counts, smoothed)
\item \code{inferred.agg}: inferred daily reports aggregated on the reporting schedule as input in \code{cl.data}
\item \code{R}: the effective R estimate (summary from ensemble)
}
}
\description{
Estimate the effective reproduction from clinical report data
}
\examples{

# -- THIS EXAMPLE TAKES ABOUT 30 SECONDS TO RUN --
# Estimate Rt

\dontrun{
# Load SARS-CoV-2 reported cases in Quebec
# during the Summer 2021
dat <- (ern::cl.data
    |> dplyr::filter(
      pt == "qc", 
      dplyr::between(date, as.Date("2021-06-01"), as.Date("2021-09-01"))
    )
)
# distributions
dist.repdelay = ern::def_dist(
    dist = 'gamma',
    mean = 5, 
    mean_sd = 1,
    sd = 1,
    sd_sd = 0.1,
    max = 10
)
dist.repfrac = ern::def_dist(
    dist = "unif",
    min = 0.1,
    max = 0.3
)
dist.incub = ern::def_dist(
    dist = "gamma",
    mean = 3.49,
    mean_sd = 0.1477,
    shape = 8.5,
    shape_sd = 1.8945,
    max = 8
)
dist.gi = ern::def_dist(
    dist = "gamma",
    mean = 6,
    mean_sd = 0.75,
    shape = 2.4,
    shape_sd = 0.3,
    max = 10
)

# settings
prm.daily <- list(
    method = "renewal",
    popsize = 8.5e6, # Q3 (July 1) 2022 estimate for Quebec
    burn = 500,
    iter = 500,
    chains = 2,
    prior_R0_shape = 1.1, prior_R0_rate = 0.6, 
    prior_alpha_shape = 1, prior_alpha_rate = 1
)
prm.daily.check <- list(
    agg.reldiff.tol = 10
)
prm.smooth <- list(
    method = "rollmean",
    align = "center",
    window = 7
)
prm.R <- list(
    iter = 20, 
    CI = 0.95, 
    window = 7, 
    config.EpiEstim = NULL
)

x <- estimate_R_cl(
  dat,
  dist.repdelay,
  dist.repfrac,
  dist.incub,
  dist.gi,
  prm.daily,
  prm.daily.check,
  prm.smooth,
  prm.R
)

# Rt estimates
print(x$R)
}

 

}
\seealso{
\code{\link[=plot_diagnostic_cl]{plot_diagnostic_cl()}} \code{\link[=estimate_R_ww]{estimate_R_ww()}}
}
