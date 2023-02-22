
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PhaseTypeR: general-purpose phase-type functions

<!-- badges: start -->

[![R-CMD-check](https://github.com/rivasiker/PhaseTypeR/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/rivasiker/PhaseTypeR/actions/workflows/check-standard.yaml)
[![CRAN](https://www.r-pkg.org/badges/version/PhaseTypeR)](https://CRAN.R-project.org/package=PhaseTypeR)
[![devel
Version](https://img.shields.io/badge/devel-1.0.5.9000-brightgreen.svg)](https://github.com/rivasiker/PhaseTypeR)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/PhaseTypeR)](https://CRAN.R-project.org/package=PhaseTypeR)
[![Codecov test
coverage](https://codecov.io/gh/rivasiker/PhaseTypeR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rivasiker/PhaseTypeR?branch=master)
<!-- badges: end -->

This package implements core functions from phase-type theory. Its
general functions are useful for a wide-ranging variety of contexts.
`PhaseTypeR` can be used to model continuous and discrete phase-type
distributions, both univariate and multivariate. The package includes
functions for outputting the mean and (co)variance of phase-type
distributions; their density, probability and quantile functions;
functions for random draws; functions for reward-transformation; and
functions for plotting the distributions as networks.

## Installation

You can install `PhaseTypeR` from CRAN via:

``` r
install.packages("PhaseTypeR")
```

If you install devtools in your R environment with
`install.packages("devtools")`, the development version of the package
can be installed with the following command:

``` r
devtools::install_github("rivasiker/PhaseTypeR")
```

These installations should handle the missing dependencies, but you can
manually install them by running the following:

``` r
install.packages("expm")
install.packages("igraph")
```

## Basic example

This is a basic example for the univariate continuous phase-type
distribution.

``` r
# Load the package
library(PhaseTypeR)

# Define the sub-intensity rate matrix
subintensity_matrix <- matrix(c(-1.5,  0,  0,
                                 1.5, -1,  0,
                                  0,   1, -0.5), ncol = 3)
# Define the initial probabilities
initial_probabilities <- c(0.9, 0.1, 0)

# Create continuous phase-type object
ph <- PH(subintensity_matrix, initial_probabilities)
```

Summary of the object:

``` r
summary(ph)
#> 
#> Subintensity matrix:
#>      [,1] [,2] [,3]
#> [1,] -1.5  1.5  0.0
#> [2,]  0.0 -1.0  1.0
#> [3,]  0.0  0.0 -0.5
#> 
#> Initial probabilities:
#>      [,1] [,2] [,3]
#> [1,]  0.9  0.1    0
#> 
#> Defect:
#> [1] 0
#> 
#> Mean: 3.6
#> 
#> Variance: 5.44
```

Density function:

``` r
dPH(1:5, ph)
#> [1] 0.1506327 0.2216989 0.1991321 0.1482987 0.1009037
```

Cumulative density function:

``` r
pPH(1:5, ph)
#> [1] 0.07030638 0.26728005 0.48232823 0.65658059 0.78032198
```

Quantile function:

``` r
qPH(c(0.25, 0.5, 0.75), ph)
#> [1] 1.921862 3.089664 4.716588
```

Random sampling:

``` r
set.seed(0)
rPH(6, ph)
#> [1] 4.648661 6.111533 4.586241 5.708529 8.354549 5.768456
```

## Learn more

You can check out the full functionality of `PhaseTypeR` and its
application to population genetics in the following guides:

- [Getting
  Started](https://rivasiker.github.io/PhaseTypeR/articles/PhaseTypeR.html)
- [Using PhaseTypeR for population
  genetics](https://rivasiker.github.io/PhaseTypeR/articles/a2_pop_gen_iker.html)
- [Using PhaseTypeR for the site frequency
  spectrum](https://rivasiker.github.io/PhaseTypeR/articles/a3_phasetypeR_SFS.html)

## Report an issue

If you have found a bug, you can file an issue at
<https://github.com/rivasiker/PhaseTypeR/issues>. Please, make sure to
read the [contribution guidelines](.github/CONTRIBUTING.md) and the
[code of conduct](.github/CODE_OF_CONDUCT.md) before making any changes.
