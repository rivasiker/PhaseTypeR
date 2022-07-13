# PhaseTypeR 1.0.3

* Fixed numeric instability of DPH functions with probabilities 
close to 0. 
* Changed var function to S3 method.
* Updated mean method to avoid masking when loading the package.
* Changed Depends to Imports.
* Added print method for phase-type objects.

# PhaseTypeR 1.0.2

* Correction of qDPH for when x <= defect.
* Changed the behavior of rMPH and rMDPH, so that all of the univariate draws
are coupled.
* Correction of the function of var for MDPH.

# PhaseTypeR 1.0.1

* Minor patch to solve no long double CRAN check (noLD).

# PhaseTypeR 1.0.0

* First CRAN release.
