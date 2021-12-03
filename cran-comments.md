## Notes

### Third submission

> tests on x86_64 Linux with R-devel configured using --disable-long-double
Error: processing vignette 'a3_phasetypeR_SFS.Rmd' failed with diagnostics:
negative probability

Tolerance added for numeric tests. Version number changed accordingly.

### Second submission

> If there are references describing the methods in your package, please 
add these in the description field of your DESCRIPTION file in the form
authors (year) 
authors (year) 
authors (year, ISBN:...)
or if those are not available:
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for 
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

Most relevant references added.

> Please always write package names, software names and API (application 
programming interface) names in single quotes in title and description. 
e.g: --> 'PhaseTypeR'

Done.

> Please always make sure to reset to user's options(), working directory 
or par() after you changed it in examples and vignettes and demos.
e.g.: inst/doc/...
oldpar <- par(mfrow = c(1,2))
...
par(oldpar)

Done.

## Test environments

* GitHub Actions (ubuntu-20.04): release, devel
* GitHub Actions (macOS-latest): release
* GitHub Actions (windows-latest): release
* R-hub builder (Fedora Linux): devel
* R-hub builder (Ubuntu Linux): release
* R-hub builder (Windows): devel
* win-builder: devel
* local R installation (macOS): R 4.1.1

## R CMD check results

0 errors | 0 warnings | 1 note

* Days since last update: 3
