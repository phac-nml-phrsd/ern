.onAttach <- function(libname, pkgname){
  suppressWarnings(j <- runjags::testjags(silent = TRUE))
  if(isFALSE(j$JAGS.found)){
    warning("JAGS is not installed on this machine but is required for Rt calculations on clinical testing data using ern::estimate_Rt_cl().
To use this functionality, please install JAGS on https://sourceforge.net/projects/mcmc-jags/files/
or request JAGS to be installed by your network administrator.
See README for more details.")
  }
  else if(isTRUE(j$JAGS.found)){
    packageStartupMessage(
    paste("JAGS version", j$JAGS.version, "installed.\nrjags version", j$rjags.version, "installed."))
  }
}

.onLoad <- function(libname, pkgname){
  suppressWarnings(j <- runjags::testjags(silent = TRUE))
  if(isFALSE(j$JAGS.found)){
    warning("JAGS is not installed on this machine but is required for Rt calculations on clinical testing data using ern::estimate_Rt_cl().
To use this functionality, please install JAGS on https://sourceforge.net/projects/mcmc-jags/files/
or request JAGS to be installed by your network administrator.
See README for more details.")
  }
}
