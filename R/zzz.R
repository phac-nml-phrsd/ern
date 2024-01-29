.onAttach <- function(libname, pkgname){
  
  msg = paste("\nern version:", utils::packageVersion("ern"),
              "\n\nIf not already installed, software JAGS is recommended.",
              "\n(https://sourceforge.net/projects/mcmc-jags/files/)\n")
  
  packageStartupMessage(msg)
  
  # `runjags`'s own check if JAGS is installed
  
  #  FIXME: the line below generates warnings that are not
  # suppressable according to `R CMD chech --as-cran`:
  # suppressWarnings(j <- runjags::testjags(silent = TRUE))
  # 
  # if(isFALSE(j$JAGS.found)){
  #   msg = paste("\n --- WARNING ---\n",
  #   "JAGS is not installed on this machine but is required\n",
  #   "for Rt calculations on clinical testing data using \n", 
  #   "ern::estimate_Rt_cl().\n",
  #   "To use this functionality, please install JAGS from\n", 
  #   "https://sourceforge.net/projects/mcmc-jags/files/ \n",
  #   "or request JAGS to be installed by your network administrator.\n",
  #   "See README for more details.")
  # }
  # else if(isTRUE(j$JAGS.found)){
  #   msg = paste(" -  JAGS version", j$JAGS.version, 
  #               "installed.\n -  rjags version", j$rjags.version, 
  #               "installed.")
  # }
  # packageStartupMessage(msg)
}

