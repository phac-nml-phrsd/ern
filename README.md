# ern
[![codecov](https://codecov.io/gh/phac-nml-phrsd/ern/branch/main/graph/badge.svg?token=SWXENVF9T4)](https://codecov.io/gh/phac-nml-phrsd/ern)

R library to estimate the Effective Reproduction Number (Rt)

`devtools::install_github('phac-nml-phrsd/ern')`

## Guidelines

 * Reference book for R packages development: https://r-pkgs.org/
 * Try to limit the number of other libraries `ern` depends on (or imports, see below). The goal of the `ern` package is to estimate $R_t$ -- nothing more, nothing less. 
 * `Imports` vs. `Depends`: **prefer `Imports`**
   * Hadley Wickham [recommends](https://r-pkgs.org/dependencies-mindset-background.html#sec-dependencies-namespace)
    * If a package is installed,
      * _Loading_ will load code, data, and any DLLs; register S3 and S4 methods; and run the `.onLoad()` function. After loading, the package is available in memory, but because it’s not in the search path, you won’t be able to access its components without using `::`. Confusingly, `::` will also load a package automatically if it isn’t already loaded.
      * _Attaching_ puts the package in the search path (Section 11.4.1). You can’t attach a package without first loading it, so both `library()` (or `require()`) load then attach the package. This also runs the `.onAttach() ` function.
    * Listing a package in either `Depends` or `Imports` ensures that it’s installed when needed. The main difference is that a package you list in `Imports` will just be loaded when you use it, whereas a package you list in `Depends` will be attached when your package is attached. Unless there is a good reason otherwise, **you should always list packages in `Imports` not `Depends`**. That’s because a good package is self-contained, and minimises changes to the global landscape, including the search path.
   * an in-depth explanation: [Suraj Gupta's blog](https://blog.thatbuthow.com/how-r-searches-and-finds-stuff/)
    * "`Depends` is less safe. `Depends` makes a package vulnerable to whatever other packages are loaded by the user."
    * `Imports` adds a secondary package’s tools directly into `imports:primary_package`, which gets searched immediately after `namespace:primary_package`. `Depends` is like using `library()` so pkg attachement order will matter and cannot necessarily be controlled by the developer (we won't know what other pkgs the user may have loaded into their session and when).



