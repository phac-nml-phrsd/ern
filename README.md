
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ern

<!-- badges: start -->

[![codecov](https://codecov.io/gh/phac-nml-phrsd/ern/branch/main/graph/badge.svg?token=SWXENVF9T4)](https://app.codecov.io/gh/phac-nml-phrsd/ern)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

This repository stores the code used in `ern`, an R package to estimate
the effective reproduction number ($R_t$) of pathogens using clinical or
wastewater data.

This package is developed at the Public Health Agency of Canada /
National Microbiology Laboratory. Please note this software is provivded
“as is”, without warranty of any kind; see the [license](LICENSE).

## Installation

To install the latest version of this package:

``` r
devtools::install_github('phac-nml-phrsd/ern')
```

### Note on JAGS

`rjags` is a dependency for `ern`, specifically for Rt calculations
performed on clinical testing data. If you are only performing
calculations using wastewater data, you do not need to worry about
installing `rjags` and can skip this section.

`rjags` is the R interface for the [`JAGS` Bayesian modelling
library](https://mcmc-jags.sourceforge.io/). Installation of `JAGS` is
required and the latest version can be found
[here](https://sourceforge.net/projects/mcmc-jags/files/). It is
recommended that you are using the latest version of R and `JAGS` for
optimal usage of `ern`.

- For users who are only unable to install the latest version of `JAGS`,
  we recommend downgrading to an R version that is less than 4.2. More
  details can be found
  [here](https://martynplummer.wordpress.com/2022/04/12/windows-update-jags-4-3-1-is-released/)
- For users who are having issues installing or running `JAGS` or
  `rjags`, we recommend consulting the [`JAGS` discussion
  board](https://sourceforge.net/p/mcmc-jags/discussion/610037/).

## Vignettes

To use `ern` to estimate $R_t$:

``` r
vignette("est-rt", package = "ern")
```

To learn more about how default distribution parameters were estimated
in `ern`:

``` r
vignette("distributions", package = "ern")
```

## Citation

You can cite `ern` with:

``` r
citation('ern')
#> 
#> To cite package 'ern' in publications use:
#> 
#>   Champredon D, Papst I, Yusuf W (2023). _ern: An R Package to Estimate
#>   the Effective Reproduction Number Using Clinical and Wastewater
#>   Surveillance Data_. National Microbiology Laboratory, Public Health
#>   Agency of Canada, Government of Canada.
#>   <https://github.com/phac-nml-phrsd/ern>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ern: An R Package to Estimate the Effective Reproduction Number Using Clinical and Wastewater Surveillance Data},
#>     author = {David Champredon and Irena Papst and Warsame Yusuf},
#>     organization = {National Microbiology Laboratory, Public Health Agency of Canada, Government of Canada},
#>     year = {2023},
#>     url = {https://github.com/phac-nml-phrsd/ern},
#>   }
```
