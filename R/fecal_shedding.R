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
    # Values obtained from Nourbakhsh et. al. (2021)
    fec = list(
      dist = "gamma",
      mean = 14.04042,
      mean_sd = NULL,
      sd = 10.07955,
      sd_sd = NULL,
      max = 36
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
