
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

  fec = NULL

  if(tolower(pathogen) == 'sarscov2'){
    y = 1:20
    z = y^2*exp(-y/2)
    fec = z/sum(z)

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
