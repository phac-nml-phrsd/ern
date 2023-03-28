.onLoad <- function(libname, pkgname){
  j = runjags::testjags(silent = TRUE)
  if(is.null(j)){
    stop("JAGS is not installed on this machine.
Please install JAGS on https://sourceforge.net/projects/mcmc-jags/files/
or request JAGS to be installed by your network administrator
before loading package.")
  }
  else{
    message(
    paste("JAGS version", j$JAGS.version, "installed.\nrjags version", j$rjags.version, "installed."))

  }
}
