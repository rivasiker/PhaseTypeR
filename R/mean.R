
#' @include phase_type.R
NULL

#' Mean of phase-type distributions
#'
#' Calculates the mean of continuous, discrete and multivariate phase-type
#' distributions, represented by the \code{cont_phase_type},
#' \code{disc_phase_type} and \code{mult_cont_phase_type} classes.
#'
#' For the univariate case (\code{cont_phase_type} and \code{disc_phase_type}),
#' the mean of the distribution is returned.
#'
#' In the case of multivariate phase-type distributions three
#' different usages can be distinguished:
#' \itemize{
#'  \item{If \code{v = NULL} (default), the means of all the variables defined by
#'  the sub-intensity matrix are returned}
#'  \item{If \code{v} is an integer, then the mean of the variable with the specified index in the
#'  reward matrix is returned.}
#'  \item{If \code{v} is a vector, then the means of the variables defined
#' by those indices will be returned.}
#' }
#'
#' @param x a \code{cont_phase_type}, \code{disc_phase_type},
#'  \code{mult_cont_phase_type} or \code{mult_disc_phase_type} object
#' @param ... other arguments passed to methods
#'
#' @rdname mean
#'
#' @export


mean <- function(x, ...) {
  UseMethod('mean', x)
}

#' mean method for \code{cont_phase_type}
#'
#' @rdname mean
#' @export

mean.cont_phase_type <- function(x, ...) {
  moment_ph(x, 1)
}

#' mean method for \code{disc_phase_type}
#'
#' @rdname mean
#' @export

mean.disc_phase_type <- function(x, ...) {
  mean <- sum(x$init_probs %*% solve(diag(nrow = nrow(x$subint_mat))
                                     - x$subint_mat))
  mean <- as.numeric(mean + x$defect)
  return(mean)
}

#' mean method for \code{mult_cont_phase_type}
#'
#' @param v NULL, integer or vector.
#'
#' @rdname mean
#' @export

mean.mult_cont_phase_type <- function(x, v = NULL, ...) {
  if (!is.null(v)) {
    if (length(v) == 1) {
      return(moment_mph(x, v))
    } else {
      result <- matrix(NA, nrow = length(v))
      for (i in 1:length(v)) {
        result[i,] <- moment_mph(x, v[i])
      }
      return(result[,1])
    }
  } else {
    result <- matrix(NA, nrow = ncol(x$reward_mat))
    for (i in 1:nrow(result)) {
      result[i,] <- moment_mph(x, i)
    }
    return(result[,1])
  }
}

mean.mult_disc_phase_type <- function(x, v = NULL, ...){
  result <- rep(0, ncol(x$reward_mat))
  for (i in 1:ncol(x$reward_mat)){
    result[i] <- mean(reward_phase_type(x, x$reward_mat[,i]))
  }
  return(result)
}
