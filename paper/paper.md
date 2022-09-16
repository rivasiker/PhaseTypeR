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

| Quantity                | Formula                                                                                | Function |
|-------------------------|----------------------------------------------------------------------------------------|----------|
| Mean                    | $\text{E}(\tau)=\boldsymbol{\pi} (-\boldsymbol{T})^{-1}\boldsymbol{e}$                 | `mean()` |
| Variance                | $\text{V}(\tau)=\text{E}(\tau^2)-\text{E}(\tau)^2$                                     | `var()`  |
| Density                 | $f(x)=\boldsymbol{\pi}\exp(\boldsymbol{T}x)(\boldsymbol{-T}\boldsymbol{e})$, $x\geq 0$ | `dPH()`  |
| Cumulative distribution | $F(x)=1-\boldsymbol{\pi}\exp(\boldsymbol{T}x)\boldsymbol{e}$, $x\geq 0$                | `pPH()`  |
| Quantile function       |                                                                                        | `qPH()`  |
| Random sampling         |                                                                                        | `rPH()`,  `rFullPH()` |


# Mathematics

Single dollars ($) are required for inline mathematics e.g. $f(x) = e^{\pi/x}$

Double dollars make self-standing equations:

$$\Theta(x) = \left\{\begin{array}{l}
0\textrm{ if } x < 0\cr
1\textrm{ else}
\end{array}\right.$$

You can also use plain \LaTeX for equations
\begin{equation}\label{eq:fourier}
\hat f(\omega) = \int_{-\infty}^{\infty} f(x) e^{i\omega x} dx
\end{equation}
and refer to \autoref{eq:fourier} from text.

# Citations

Citations to entries in paper.bib should be in
[rMarkdown](http://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html)
format.

If you want to cite a software repository URL (e.g. something on GitHub without a preferred
citation) then you can do it with the example BibTeX entry below for @fidgit.

For a quick reference, the following citation commands can be used:
- `@author:2001`  ->  "Author et al. (2001)"
- `[@author:2001]` -> "(Author et al., 2001)"
- `[@author1:2001; @author2:2001]` -> "(Author1 et al., 2001; Author2 et al., 2002)"

# Figures

Figures can be included like this:
![Caption for example figure.\label{fig:example}](figure.png)
and referenced from text using \autoref{fig:example}.

Figure sizes can be customized by adding an optional second parameter:
![Caption for example figure.](figure.png){ width=20% }

# Acknowledgements

We acknowledge contributions from Brigitta Sipocz, Syrtis Major, and Semyeong
Oh, and support from Kathryn Johnston during the genesis of this project.

# References
