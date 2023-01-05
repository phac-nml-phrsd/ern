# ern
R library to estimate the Effective Reproduction Number (Rt)

`devtools::install_github('phac-nml-phrsd/ern')`




## Guidelines

 * Reference book for R packages development: https://r-pkgs.org/
 * Try to limit the number of other libraries `ern` depends on (or imports, see below). The goal of the `ern` package is to estimate $R_t$ -- nothing more, nothing less. 
 * `Imports` vs. `Depends`: **prefer `Imports`**
   * Hadley Wickham [recommends](https://r-pkgs.org/dependencies-mindset-background.html#sec-dependencies-namespace): "[...] Unless there is a good reason otherwise, you should always list packages in Imports not Depends. [...] "
   * an in-depth explanation: [Suraj Gupta's blog](https://blog.thatbuthow.com/how-r-searches-and-finds-stuff/)
    * "`Depends` is less safe. `Depends` makes a package vulnerable to whatever other packages are loaded by the user."
    * `Imports` adds a secondary package’s tools directly into `imports:primary_package`, which gets searched immediately after `namespace:primary_package`. `Depends` is like using `library()` so pkg attachement order will matter and cannot necessarily be controlled by the developer (we won't know what other pkgs the user may have loaded into their session and when).



