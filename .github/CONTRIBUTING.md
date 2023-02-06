# Contributing to PhaseTypeR

This outlines how to propose a change to PhaseTypeR. 

### Did you find a typo?

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

### Did you find a bigger mistake?

If you found a bug and want to make a change, you can file a issue, and the PhaseTypeR team will review your petition. You the issue can be filed at <https://github.com/rivasiker/PhaseTypeR/issues> by writing a minimal example that illustrates the bug. 

If you want to fix a bug yourself by changing the code, you can contribute to the PhaseTypeR project by creating a pull request with the proposed fix. You can take the following steps:

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("rivasiker/PhaseTypeR", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). 

Please, make sure to keep the code style using the following guidelines:

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests.  

### Do you want to add a new feature or enhance an existing one?

Please, contact the PhaseTypeR team. We'll be happy to discuss your thoughts!

### Code of Conduct

Please note that the PhaseTypeR project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.
