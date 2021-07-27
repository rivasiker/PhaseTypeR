# PhaseTypeR: general-purpose phase-type functions

<!-- badges: start -->
[![R-CMD-check](https://github.com/rivasiker/PhaseTypeR/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/rivasiker/PhaseTypeR/actions/workflows/check-standard.yaml)
[![Codecov test coverage](https://codecov.io/gh/rivasiker/PhaseTypeR/branch/master/graph/badge.svg)](https://codecov.io/gh/rivasiker/PhaseTypeR?branch=master)
  <!-- badges: end -->


This package implements core functions from phase-type theory. The package contains general functions, so it is useful for a wide-ranging variety of contexts. 

## Installation

If you install devtools in your R environment with `install.packages("devtools")`, the development version of the package can be installed with the following command:

``` r
devtools::install_github("rivasiker/PhaseTypeR")
```

## Basic example

``` r
library(PhaseTypeR)

subintensity_matrix <- matrix(c(-1.5,  0,  0,
                                 1.5, -1,  0,
                                   0,  1, -0.5), ncol = 3)
initial_probabilities <- c(0.9, 0.1, 0)

ph <- PH(subintensity_matrix, initial_probabilities)

summary(ph)
```

```
Subintensity matrix:
     [,1] [,2] [,3]
[1,] -1.5  1.5  0.0
[2,]  0.0 -1.0  1.0
[3,]  0.0  0.0 -0.5

Initial probabilities:
     [,1] [,2] [,3]
[1,]  0.9  0.1    0

Defect:
[1] 0

Mean: 3.6

Variance: 5.44
```

## Learn more

patchwork can do so much more. Check out the guides for learning
everything there is to know about all the different features:

You can check out the full functionality of `PhaseTypeR` and its application to 
population genetics in the following guides:

  - [Getting
    Started](https://rivasiker.github.io/PhaseTypeR/articles/PhaseTypeR.html)
  - [Using PhaseTypeR for population genetics](https://rivasiker.github.io/PhaseTypeR/articles/pop_gen_iker.html)
