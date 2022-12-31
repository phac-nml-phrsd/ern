
#' Retrieve the Generation Interval Distribution
#'
#' @param pathogen String. Name of the pathogen.
#' @param subtype String. Oprion subtype
#'
#' @return A list.
#' @export
#'
#' @examples
#' gi = get_gi('sarscov2')
#' print(gi)
#'
get_gi <- function(pathogen, subtype) {

  res = NULL

  if(tolower(pathogen)=='sarscov2'){
    res = list(
      # copied over from EpiNow2
      # EpiNow2::get_generation_time(disease = "SARS-CoV-2",source = "ganyani")
      dist = "gamma",
      mean = 3.635272,
      mean_sd = 0.7109351,
      sd = 3.07531,
      sd_sd = 0.7695178,
      max = 15
    )
  }

  if(is.null(res)) stop(paste0("generation interval not defined for '",
                               pathogen, "'"))
  return(res)

}
