#' @title Fecal Shedding Distribution
#'
#' @param pathogen String. Name of the pathogen ('sarscov2', 'influenza', 'RSV')
#' @param subtype String.
#'
#' @return A numerical vector where each element represent one day since infection.
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
    shed.asymp = exp(c(5,7.30103,7.083333,6.677121,6.30,5.9))
    shed.symp = exp(c(5,7.30103,7.083333,6.677121,6.30,5.9))
    shed.noninf = exp(c(5.40103,4.60103,3.90103,3.10103,2.10103,1.2))

    fec = rowMeans(cbind(shed.asymp, shed.noninf, shed.symp))/
      sum(rowMeans(cbind(shed.asymp, shed.noninf, shed.symp)))

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
