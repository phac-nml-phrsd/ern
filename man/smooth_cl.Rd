% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/smooth.R
\name{smooth_cl}
\alias{smooth_cl}
\title{Smooth realizations from estimating daily reports}
\usage{
smooth_cl(cl.daily, prm.smooth)
}
\arguments{
\item{cl.daily}{Data frame. Output of \code{\link[=agg_to_daily]{agg_to_daily()}}.}

\item{prm.smooth}{List. Smoothing parameters for input signal (daily reports) into Rt calculation. Elements include:
\itemize{
\item \code{method}: smoothing method to use; currently only \code{rollmean} (centred rolling average) is implemented
\item \code{window}: width of smoothing window (number of days)
}
Set this entire argument to \code{NULL} to turn off smoothing.}
}
\value{
Data frame
}
\description{
Smooth realizations from estimating daily reports
}
\keyword{internal}
