#' @title Fecal Shedding Gamma Distribution
#'
#' @description Function generates a gamma distribution for each fecal shedding
#'  profile.
#'
#' @param fec.list List of fecal shedding parameters based on various shedding
#'  profiles (e.g. asymptomatic, symptomatic, and hospitalized symptomatic).
#'
#' @return A dataframe containing the mean and sd of gamma distributions for
#'  each shedding profile.
#'
#' @export
#'
get_gamma_fec <- function(fec.list){
  log_likelihood = function(par, data){
    a = par[1]
    s = par[2]
    return(-sum(data$y*(dgamma(data$x, shape = a, scale = s, log = TRUE))))
  }
  f = list()
  for(i in 1:length(fec.list)){
    fec.list[[i]][["y"]] = fec.list[[i]][["y"]]/sum(fec.list[[i]][["y"]])
    res = optim(par = c(3, 6), fn = log_likelihood,
                data = fec.list[[i]])
    y = dgamma(fec.list[[i]][["x"]], shape = res$par[1], scale = res$par[2])
    f[[i]] = data.frame(
      mean = numeric(),
      sd = numeric()
    )
    f[[i]][nrow(f[[i]]) +1,] = c(mean(y), sd(y))
  }

  df = dplyr::bind_rows(f)
  return(df)
}

#' @title Fecal Shedding Distribution
#'
#' @param pathogen String. Name of the pathogen ('sarscov2', 'influenza', 'RSV')
#' @param subtype String.
#'
#' @return A fecal shedding distribution stored as a list.
#' @export
#'
#' @examples
#' fec = get_fecal_shedding('sarscov2')
#' print(fec)
#'
get_fecal_shedding <- function(pathogen, subtype = '') {

  # Check if correct pathogen specified
  if(!isTRUE(pathogen %in% c('sarscov2', 'influenza', 'rsv'))){
    stop("Pathogen not found. Aborting!")
  }

  fec = NULL

  if(tolower(pathogen) == 'sarscov2'){
    # Specify sarscov2 shedding profiles. Values obtained from
    # Nourbakhsh et. al. (2021)
    shed.asymp = data.frame(
      x = c(seq(from=1,to=10,by=2), seq(from=11,to=34,by=4)),
      y = c(5,7.30103,7.083333,6.677121,6.30,
            5.40103,4.60103,3.90103,3.10103,2.10103,1.2)
    )
    shed.symp = data.frame(
      x = c(seq(from=1,to=12,by=2), seq(from=13,to=36,by=4)),
      y = c(5,7.30103,7.083333,6.677121,6.30,5.9,
            5.40103,4.60103,3.90103,3.10103,2.10103,1.2)
    )
    shed.hosp = data.frame(
      x = c(seq(from=1,to=8,by=2)),
      y = c(5,7.30103,7.083333,6.677121)
    )

    fec.l = list(
      shed.asymp,
      shed.symp,
      shed.hosp
    )

    fec.gam = get_gamma_fec(fec.l)

    fec = list(
      dist = "gamma",
      mean = mean(fec.gam$mean),
      mean_sd = sd(fec.gam$mean),
      sd = mean(fec.gam$sd),
      sd_sd = sd(fec.gam$sd),
      max = max(fec.gam$mean)
    )

    if(subtype == 'foo'){
      # TO DO...
    }
  }


  if(tolower(pathogen) == 'influenza'){
    y = 1:15
    z = y^2*exp(-y/2)
    fec = z/sum(z)

    if(subtype == 'foo'){
      # TO DO...
    }
  }

  if(tolower(pathogen) == 'rsv'){
    y = 1:10
    z = y^2*exp(-y/3)
    fec = z/sum(z)

    if(subtype == 'foo'){
      # TO DO...
    }
  }

  return(fec)
}
