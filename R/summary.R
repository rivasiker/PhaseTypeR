#' Pretty summary of the \code{cont_phase_type} class.
#'
#'
#' @param object a cont_phase_type object
#' @param ... other arguments passed to methods
#'
#' @return This function prints a nicely-formatted summary of
#' a \code{cont_phase_type} object. The summary includes the
#' sub-intensity matrix, the initial probabilities, the defect,
#' the mean and the variance of the phase-type object.
#'
#' @examples
#'
#' ph <- PH(matrix(c(-3, 0, 1,
#'                2, -3, 1,
#'                1, 1, -2), ncol = 3))
#'
#' summary(ph)
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

#' Pretty summary of the \code{disc_phase_type} class.
#'
#' @param object a disc_phase_type object
#' @param ... other arguments passed to methods
#'
#' @return This function prints a nicely-formatted summary of
#' a \code{disc_phase_type} object. The summary includes the
#' sub-intensity matrix, the initial probabilities, the defect,
#' the mean and the variance of the phase-type object.
#'
#' @examples
#'
#' dph <- DPH(matrix(c(0.4, 0, 0.2,
#'                     0.5, 0.3, 0.2,
#'                     0, 0.7, 0.2), ncol = 3))
#'
#' summary(dph)
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

#' Pretty summary of the \code{mult_cont_phase_type} class.
#'
#' @param object a mult_cont_phase_type object
#' @param ... other arguments passed to methods
#'
#' @return This function prints a nicely-formatted summary of
#' a \code{mult_cont_phase_type} object. The summary includes the
#' sub-intensity matrix, the initial probabilities, the defect,
#' the reward matrix,
#' the mean and the (co)variance of the phase-type object.
#'
#' @examples
#'
#' subint <- matrix(c(-3, 0, 1,
#'                     2, -3, 1,
#'                     1, 1, -2), ncol = 3)
#' R <- matrix(c(0, 1, 1,  2,
#'                  2, 1, 5,  2,
#'                  0, 1, 10, 2), nrow = 3, ncol=4, byrow=TRUE)
#' mph <- MPH(subint, reward_mat = R)
#'
#' summary(mph)
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
  cat('\nMeans:\n')
  print(mean(object))
  cat('\nVariance-covariance matrix:\n')
  print(var(object))
  cat('\n\n')
}

#' Pretty summary of the \code{mult_dist_phase_type} class.
#'
#' @param object a mult_dist_phase_type object
#' @param ... other arguments passed to methods
#'
#' @return This function prints a nicely-formatted summary of
#' a \code{mult_dist_phase_type} object. The summary includes the
#' sub-intensity matrix, the initial probabilities, the defect,
#' the reward matrix,
#' the mean and the (co)variance of the phase-type object.
#'
#' @examples
#'
#' subint <- matrix(c(0.4, 0, 0.2,
#'                    0.5, 0.3, 0.2,
#'                    0, 0.7, 0.2), ncol = 3)
#' R <- matrix(c(0, 1, 1,
#'               2, 1, 5,
#'               0, 1, 10,
#'               1, 2, 3), nrow = 3)
#' mdph <- MDPH(subint, reward_mat = R)
#'
#' summary(mdph)
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
  cat('\nMeans:\n')
  print(mean(object))
  cat('\nVariance-covariance matrix:\n')
  print(var(object))
  cat('\n\n')
}
