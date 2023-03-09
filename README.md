# ern

<!-- badges: start -->

[![codecov](https://codecov.io/gh/phac-nml-phrsd/ern/branch/main/graph/badge.svg?token=SWXENVF9T4)](https://codecov.io/gh/phac-nml-phrsd/ern) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

This repository stores the code used in `ern`, an R package to estimate the effective reproduction number ($R_t$) of pathogens using clinical or wastewater data.

This package is developed at the Public Health Agency of Canada / National Microbiology Laboratory. Please note this software is provivded "as is", without warranty of any kind; see the [license](LICENSE).

## Installation

To install the latest version of this package:

```r
devtools::install_github('phac-nml-phrsd/ern')
```

## Vignettes

This package contains vignettes on how `ern` can be used to estimate $R_t$. To estimate $R_t$ using clinical data:

```r
vignette("clin-rt", package = "ern") # TODO: replace "clin-rt" with filename of cl vignette
```

To estimate $R_t$ using wastewater data:

```r
vignette("ww-rt", package = "ern")
```

To learn more about how parameters were estimated in `ern`:

```r
vignette("distributions", package = "ern")
```
