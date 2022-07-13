#' Print method for phase-type objects
#'
#' Print method for \code{cont_phase_type}, \code{disc_phase_type},
#' \code{mult_cont_phase_type} and \code{mult_disc_phase_type} classes.
#'
#' @param x phase-type object
#' @param ... other arguments not used by this method
#'
#' @return Prints the phase-type object as a list.
#'
#' @examples
#' subintensity_matrix <- matrix(c(-1.5, 1.5, 0,
#'                                  0,  -1,   1,
#'                                  0,   0,  -0.5),
#'                               ncol = 3,
#'                               byrow = TRUE)
#' ph1 <- PH(subintensity_matrix)
#'
#' print(ph1)
#'
#' @rdname print.cont_phase_type
#'
#' @export

print.cont_phase_type <- function(x, ...) {
  print.default(x, ...)
}

#' @rdname print.cont_phase_type
#'
#' @export

print.disc_phase_type <- function(x, ...) {
  print.default(x, ...)
}

#' @rdname print.cont_phase_type
#'
#' @export

print.mult_cont_phase_type <- function(x, ...) {
  print.default(x, ...)
}

#' @rdname print.cont_phase_type
#'
#' @export

print.mult_disc_phase_type <- function(x, ...) {
  print.default(x, ...)
}
