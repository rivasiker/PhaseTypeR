---
title: 'PhaseTypeR: an R package for phase-type distributions in population genetics'
tags:
  - R
  - phase-type distributions
  - population genetics
  - ancestral process
  - coalescent theory
authors:
  - name: Iker Rivas-González
    orcid: 0000-0002-0515-0628
    equal-contrib: true
    corresponding: true
    affiliation: 1
  - name: Lars Nørvand Andersen
    orcid: 0000-0001-6196-8427
    equal-contrib: true
    affiliation: 2
  - name: Asger Hobolth
    orcid: 0000-0003-4056-1286
    equal-contrib: true
    affiliation: 2
affiliations:
 - name: Bioinformatics Research Centre, Aarhus University, Denmark
   index: 1
 - name: Department of Mathematics, Aarhus University, Denmark
   index: 2
date: 16 September 2022
bibliography: paper.bib
---

# Summary

Phase-type distributions describe the time until absorption of a continuous or 
discrete-time Markov chain [@bladt2017matrix]. The probabilistic properties of 
phase-type distributions (i.e., the probability density function, cumulative 
distribution function, quantile function, moments and generating functions) 
are well-described and analytically tractable using matrix manipulations. 

Phase-type distributions have been traditionally used in actuarial sciences 
and queuing theory, and more recently in population genetics. In order to 
facilitate the use of phase-type theory in population genetics, we present
`PhaseTypeR`, a general-purpose and user-friendly R package which contains
all key functions &mdash;mean, (co)variance, probability density function, 
cumulative distribution function, quantile function and random sampling&mdash;
for both continuous and discrete phase-type distributions. Additionally, we 
also implement reward transformations, together with fuctions for multivariate 
continuous and multivariate discrete phase-type distributions. 

# Statement of need

In recent years, the usefulness of phase-type theory in population genetics has
become evident. Important quantities in population genetics such as the time until the
most recent ancestor, the total tree length, the total number of segregating sites, and 
the site frequency spectrum follow phase-type distributions [@hobolth2019phase]. 
There are already several other R packages that model phase-type distributions, such as
`actuar` [@dutang2008actuar], `mapfit` [@okamura2015mapfit; @okamura_dohi_2015; @okamura_dohi_2016] 
or `matrixdist` [@albrecher_bladt_2019; @AlbrecherBladtYslas2020]. However, these packages 
only model univariate continuous phase-type distributions, and they do not include reward 
transformations, which are particularly useful in population genetics. Moreover, these packages 
are tailored to actuarial sciences and queuing theory, which makes their implementation of 
phase-type distributions hard to use for population genetics.

`PhaseTypeR` bridges the gap between population geneticists and phase-type theory 
by enabling the use of phase-type distributions via easy-to-use R
functions. The package has already been used in @HBA2021 to model the site
frequency spectrum using multivariate phase-type theory, and we believe that its 
intuitive implementation will encourange more population geneticists to use phase-type
theory. 

# Overview

| Quantity                | Formula                                                                                | Function |
|-------------------------|----------------------------------------------------------------------------------------|----------|
| Generator function      | $\tau\sim\text{PH}(\boldsymbol{a}, \boldsymbol{T})$                                    | `PH(T, a)` |
| Mean                    | $\text{E}(\tau)=\boldsymbol{a} (-\boldsymbol{T})^{-1}\boldsymbol{e}$                   | `mean(PH)` |
| Variance                | $\text{V}(\tau)=\text{E}(\tau^2)-\text{E}(\tau)^2$                                     | `var(PH)`  |
| Density                 | $f(x)=\boldsymbol{a}\exp(\boldsymbol{T}x)(\boldsymbol{-T}\boldsymbol{e})$, $x\geq 0$   | `dPH(x, PH)`  |
| Cumulative distribution | $F(x)=1-\boldsymbol{a}\exp(\boldsymbol{T}x)\boldsymbol{e}$, $x\geq 0$                  | `pPH(x, PH)`  |
| Quantile function       |                                                                                        | `qPH(x, PH)`  |
| Random sampling         |                                                                                        | `rPH(n, PH)`,  `rFullPH(n, PH)` |
| Reward transformation   | See @bladt2017matrix                                                                   | `reward_phase_type(PH, R)` |

| Quantity                | Formula                                                                                | Function |
|-------------------------|----------------------------------------------------------------------------------------|----------|
| Generator function      | $\tau\sim\text{DPH}(\boldsymbol{a}, \boldsymbol{T})$                                   | `DPH(T, a)` |
| Mean                    | $\text{E}(\tau)=\boldsymbol{\pi} (\boldsymbol{I}-\boldsymbol{T})^{-1}\boldsymbol{e}$   | `mean(DPH)` |
| Variance                | $\text{V}(\tau)= \text{E}(\tau^2)-\text{E}(\tau)^2$                                    | `var(DPH)`  |
| Density                 | $f(x)=\boldsymbol{\pi T}^{x-1}\boldsymbol{t}$, $x\geq 1$                               | `dDPH(x, DPH)`  |
| Cumulative distribution | $F(x)=1-\boldsymbol{\pi T}^x\boldsymbol{e}$, $x\geq 1$                                 | `pDPH(x, DPH)`  |
| Quantile function       |                                                                                        | `qDPH(x, DPH)`  |
| Random sampling         |                                                                                        | `rDPH(n, DPH)`,  `rFullDPH(n, DPH)` |
| Reward transformation   | See @navarro2019discrete                                                               | `reward_phase_type(DPH, R)` |


# An example: the coalescent with recombination

![Caption for example figure.](figure.png){ width=20% }

```r
recomb_rate <- 0.3
ARG_subint_mat <- function(recomb_rate) {
  matrix(
    c(-(1+2*recomb_rate/2), 2*recomb_rate/2,  0,             0,  0,
        1,                -(3+recomb_rate/2), recomb_rate/2, 1,  1,
        0,                  4,               -6,             1,  1,
        0,                  0,                0,            -1,  0,
        0,                  0,                0,             0, -1),
    nrow=5, byrow=TRUE)
}
subintensity_matrix <- ARG_subint_mat(recomb_rate)
initial_probabilities <- c(1, 0, 0, 0, 0)
# T_left: T_MRCA in left locus
reward_left <- c(1, 1, 1, 0, 1)
# T_right: T_MRCA in right locus
reward_right <- c(1, 1, 1, 1, 0)
# Joint distribution T_joint of T_left and T_right
T_joint <- MPH(subintensity_matrix,
               initial_probabilities,
               matrix(c(reward_left, reward_right), nrow = 5))

## Simulation from the joint distribution
subintensity_matrix_09 <- ARG_subint_mat(0.166)
Tab_09 <- MPH(subintensity_matrix_09, initial_probabilities,
              matrix(c(reward_left, reward_right), nrow=5))
subintensity_matrix_01 <- ARG_subint_mat(11.316)
Tab_01 <- MPH(subintensity_matrix_01, initial_probabilities,
              matrix(c(reward_left, reward_right), nrow=5))
set.seed(3)
rTab_09 <- rMPH(1000, Tab_09)
rTab_01 <- rMPH(1000, Tab_01)
```

# References
