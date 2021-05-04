#' @describeIn generator_functions pretty summary of the \code{cont_phase_type} class.
#'
#'
#' @param object a phase-type object
#' @param ... other arguments passed to methods
#'
#' @export

summary.cont_phase_type <- function(object, ...) {
  cat('\nSubintensity matrix:\n')
  print(object$subint_mat)
  cat('\nInitial probabilities:\n')
  print(object$init_probs)
  cat('\nDefect:\n')
  print(object$defect)
  cat('\nMean: ', mean(object), '\n', sep = '')
  cat('\nVariance: ', var(object), '\n\n', sep = '')
}

#' @describeIn generator_functions pretty summary of the \code{disc_phase_type} class.
#'
#' @export

summary.disc_phase_type <- function(object, ...) {
  cat('\nSubintensity matrix:\n')
  print(object$subint_mat)
  cat('\nInitial probabilities:\n')
  print(object$init_probs)
  cat('\nDefect:\n')
  print(object$defect)
  cat('\nMean: ', mean(object), '\n', sep = '')
  cat('\nVariance: ', var(object), '\n\n', sep = '')
}

#' @describeIn generator_functions pretty summary of the \code{mult_cont_phase_type} class.
#'
#' @export

summary.mult_cont_phase_type <- function(object, ...) {
  cat('\nSubintensity matrix:\n')
  print(object$subint_mat)
  cat('\nReward matrix:\n')
  print(object$reward_mat)
  cat('\nInitial probabilities:\n')
  print(object$init_probs)
  cat('\nDefect:\n')
  print(object$defect)
}

#' @describeIn generator_functions pretty summary of the \code{mult_dist_phase_type} class.
#'
#' @export

summary.mult_disc_phase_type <- function(object, ...) {
  cat('\nSubintensity matrix:\n')
  print(object$subint_mat)
  cat('\nReward matrix:\n')
  print(object$reward_mat)
  cat('\nInitial probabilities:\n')
  print(object$init_probs)
  cat('\nDefect:\n')
  print(object$defect)
}
