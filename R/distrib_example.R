

#' Example of distributions for a given virus
#'
#' @param virus String. Name of the virus. 
#' Currently implemented values are  
#' \code{COVID}, \code{FLUA}, \code{FLUB}, \code{RSV}
#'
#' @returns A list of distributions (generation interval, incubation period,
#' fecal shedding).
#' 
#' @export
#'
#' @examples
#' dist_bulk_example('COVID')
#' 
#' 
dist_bulk_example <- function(virus) {
  v = toupper(virus)
  if(! v %in% c('COVID', 'FLUA', 'FLUB', 'RSV'))
    stop('Function dist_bulk_example(): virus `',virus,'` not implemented.')
  
  if(v == 'COVID') return(dist_bulk_example_COVID())
  if(v == 'FLUA')  return(dist_bulk_example_FLUA())
  if(v == 'FLUB')  return(dist_bulk_example_FLUB())
  if(v == 'RSV')   return(dist_bulk_example_RSV())
}


#' Helper function.
#'
#' @keywords internal
#' 
dist_bulk_example_COVID <- function() {
  
  # * Warning * 
  # Lists below are generated using AI agent. Not fully verified by a human.
  
  # SARS-CoV-2 incubation period, serial interval, and generation interval references
  #
  # FirstAuthor| Year | DOI
  # -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # Lauer      | 2020 | 10.7326/M20-0504
  # McAloon    | 2020 | 10.1136/bmjopen-2020-039652
  # Wei        | 2021 | 10.1001/jamanetworkopen.2021.24131
  # Nishiura   | 2020 | 10.1016/j.ijid.2020.02.060
  # Du         | 2020 | 10.3201/eid2606.200357
  # Ali        | 2020 | 10.1126/science.abc9004
  # Rai        | 2021 | 10.1093/cid/ciaa1046
  # Ferretti   | 2020 | 10.1126/science.abb6936
  # Ganyani    | 2020 | 10.2807/1560-7917.ES.2020.25.17.2000257
  # Hart       | 2021 | 10.1371/journal.pbio.3001118
  # Sun        | 2021 | 10.1038/s41591-021-01442-0
  #
  # Most commonly cited for transmission modelling:
  # Incubation period:
  #   Lauer et al. (2020)        DOI: 10.7326/M20-0504
  #
  # Serial interval:
  #   Nishiura et al. (2020)     DOI: 10.1016/j.ijid.2020.02.060
  #   Du et al. (2020)           DOI: 10.3201/eid2606.200357
  #
  # Generation interval:
  #   Ganyani et al. (2020)      DOI: 10.2807/1560-7917.ES.2020.25.17.2000257
  #   Ferretti et al. (2020)     DOI: 10.1126/science.abb6936
  #   Hart et al. (2021)         DOI: 10.1371/journal.pbio.3001118
  
  dist.gi = def_dist(
    dist    = "gamma",
    mean    = 5,
    mean_sd = 0.75,
    sd      = 2.25,
    sd_sd   = 0.3,
    max     = 10
  )
  # plot_dist(dist.gi)  # <- visual check
  
  dist.incub = ern::def_dist(
    dist      = "gamma",
    mean      = 5,
    mean_sd   = 0.75,
    sd        = 2.0,
    sd_sd     = 0.5,
    max       = 12)
  # plot_dist(dist.incub)  # <- visual check
  
  
  dist.fec = ern::def_dist(
    dist = "gamma",
    mean    = 10,
    mean_sd = 4,
    sd      = 5,
    sd_sd   = 2,
    max     = 30
  )
  # plot_dist(dist.fec)  # <- visual check
  
  out = list(
    gi = dist.gi, 
    incub = dist.incub,
    fec = dist.fec
  )
  return(out)
}


#' Helper function.
#'
#' @keywords internal
#' 
dist_bulk_example_FLUA <- function() {
  
  # * Warning *
  # Lists below are generated using AI agent. Not fully verified by a human.
  
  # Seasonal influenza incubation period, serial interval,
  # and generation interval references
  #
  # FirstAuthor| Year | DOI
  # -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # Carrat     | 2008 | 10.1016/S0140-6736(08)60069-4
  # Vink       | 2014 | 10.1371/journal.pone.0094130
  # Lessler    | 2009 | 10.1016/S1473-3099(09)70069-6
  # Cowling    | 2009 | 10.3201/eid1509.090299
  # Lau        | 2015 | 10.1093/aje/kwv106
  #
  # Most commonly cited for transmission modelling:
  #
  # Incubation period:
  #   Lessler et al. (2009)      DOI: 10.1016/S1473-3099(09)70069-6
  #   Carrat et al. (2008)       DOI: 10.1016/S0140-6736(08)60069-4
  #
  # Serial interval:
  #   Cowling et al. (2009)      DOI: 10.3201/eid1509.090299
  #   Vink et al. (2014)         DOI: 10.1371/journal.pone.0094130
  #
  # Generation interval:
  #   Lau et al. (2015)          DOI: 10.1093/aje/kwv106
  
  dist.gi = def_dist(
    dist    = "gamma",
    mean    = 3.0,
    mean_sd = 0.5,
    sd      = 1.5,
    sd_sd   = 0.25,
    max     = 8
  )
  # plot_dist(dist.gi)  # <- visual check
  
  dist.incub = ern::def_dist(
    dist      = "gamma",
    mean      = 1.8,
    mean_sd   = 0.3,
    sd        = 0.8,
    sd_sd     = 0.2,
    max       = 5
  )
  # plot_dist(dist.incub)  # <- visual check
  
  dist.fec = ern::def_dist(
    dist = "gamma",
    mean    = 10,
    mean_sd = 4,
    sd      = 5,
    sd_sd   = 2,
    max     = 30
  )
  # plot_dist(dist.fec)  # <- visual check
  
  out = list(
    gi     = dist.gi,
    incub  = dist.incub,
    fec    = dist.fec
  )
  
  return(out)
}

#' Helper function.
#'
#' @keywords internal
#' 
dist_bulk_example_FLUB <- function() {
  
  # * Warning *
  # Lists below are generated using AI agent. Not fully verified by a human.
  
  # Influenza B incubation period, serial interval,
  # and generation interval references
  #
  # FirstAuthor| Year | DOI
  # -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # Carrat     | 2008 | 10.1016/S0140-6736(08)60069-4
  # Vink       | 2014 | 10.1371/journal.pone.0094130
  # Lessler    | 2009 | 10.1016/S1473-3099(09)70069-6
  # Cowling    | 2009 | 10.3201/eid1509.090299
  # Lau        | 2015 | 10.1093/aje/kwv106
  #
  # Notes:
  #   Relatively few studies estimate Influenza B-specific
  #   generation intervals. Seasonal influenza A and B are
  #   often parameterized identically in transmission models.
  #
  # Most commonly cited for transmission modelling:
  #
  # Incubation period:
  #   Lessler et al. (2009)      DOI: 10.1016/S1473-3099(09)70069-6
  #   Carrat et al. (2008)       DOI: 10.1016/S0140-6736(08)60069-4
  #
  # Serial interval:
  #   Cowling et al. (2009)      DOI: 10.3201/eid1509.090299
  #   Vink et al. (2014)         DOI: 10.1371/journal.pone.0094130
  #
  # Generation interval:
  #   Lau et al. (2015)          DOI: 10.1093/aje/kwv106
  
  dist.gi = def_dist(
    dist    = "gamma",
    mean    = 3.5,
    mean_sd = 0.6,
    sd      = 1.8,
    sd_sd   = 0.3,
    max     = 10
  )
  # plot_dist(dist.gi)  # <- visual check
  
  dist.incub = ern::def_dist(
    dist      = "gamma",
    mean      = 2.0,
    mean_sd   = 0.3,
    sd        = 0.9,
    sd_sd     = 0.2,
    max       = 6
  )
  # plot_dist(dist.incub)  # <- visual check
  
  dist.fec = ern::def_dist(
    dist = "gamma",
    mean    = 10,
    mean_sd = 4,
    sd      = 5,
    sd_sd   = 2,
    max     = 30
  )
  # plot_dist(dist.fec)  # <- visual check
  
  out = list(
    gi    = dist.gi,
    incub = dist.incub,
    fec   = dist.fec
  )
  
  return(out)
}


#' Helper function.
#'
#' @keywords internal
#' 
dist_bulk_example_RSV <- function() {
  
  # * Warning *
  # Lists below are generated using AI agent. Not fully verified by a human.
  
  # Respiratory Syncytial Virus (RSV) incubation period,
  # serial interval, and generation interval references
  #
  # FirstAuthor| Year | DOI
  # -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # Hall       | 2001 | 10.1093/clinids/33.6.792
  # Lessler    | 2009 | 10.1016/S1473-3099(09)70069-6
  # Okiro      | 2012 | 10.1093/infdis/jir469
  # Munywoki   | 2014 | 10.1093/infdis/jiu437
  # Klick      | 2011 | 10.1093/infdis/jir188
  #
  # Most commonly cited for transmission modelling:
  #
  # Incubation period:
  #   Hall et al. (2001)         DOI: 10.1093/clinids/33.6.792
  #   Lessler et al. (2009)      DOI: 10.1016/S1473-3099(09)70069-6
  #
  # Serial interval:
  #   Munywoki et al. (2014)     DOI: 10.1093/infdis/jiu437
  #
  # Generation interval:
  #   Klick et al. (2011)        DOI: 10.1093/infdis/jir188
  #   Okiro et al. (2012)        DOI: 10.1093/infdis/jir469
  
  dist.gi = def_dist(
    dist    = "gamma",
    mean    = 6.0,
    mean_sd = 1.0,
    sd      = 2.0,
    sd_sd   = 0.5,
    max     = 15
  )
  # plot_dist(dist.gi)  # <- visual check
  
  dist.incub = ern::def_dist(
    dist      = "gamma",
    mean      = 4.5,
    mean_sd   = 0.75,
    sd        = 1.5,
    sd_sd     = 0.4,
    max       = 10
  )
  # plot_dist(dist.incub)  # <- visual check
  
  dist.fec = ern::def_dist(
    dist = "gamma",
    mean    = 10,
    mean_sd = 4,
    sd      = 5,
    sd_sd   = 2,
    max     = 30
  )
  # plot_dist(dist.fec)  # <- visual check
  
  out = list(
    gi    = dist.gi,
    incub = dist.incub,
    fec   = dist.fec
  )
  
  return(out)
}


#' Examples for reporting distributions.
#'
#' @param jurisdiction String. Jurisdiction for which the reporting distributions are typical.
#' Only "Canada" is currently implemented.
#'
#' @returns List of reporting distributions (delay and fraction).
#' @export
#'
#' @examples
#' dist_bulk_example_reporting("Canada")
#'
dist_bulk_example_reporting <- function(jurisdiction) {
  
  out = NULL
  
  if(! jurisdiction %in% c('Canada'))
    stop('Function dist_bulk_example_reporting(): Only `Canada` jurisdiction is implemented.')
  
  if(jurisdiction == 'Canada'){
    dist.repdelay = ern::def_dist(
      dist = 'gamma',
      mean = 5, 
      mean_sd = 1,
      sd = 1,
      sd_sd = 0.1,
      max = 10
    )
    # plot_dist(dist.repdelay)  # <- visual check
    dist.repfrac = ern::def_dist(
      dist = "unif",
      min = 0.02,
      max = 0.10
    )
  }
  
  out = list(
    repdelay = dist.repdelay,
    repfrac  = dist.repfrac
  )
  return(out)
}

