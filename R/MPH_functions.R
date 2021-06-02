#' The Multivariate Continuous Phase-Type Distribution
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param obj an object of class \code{mult_cont_phase_type}.
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'  the number required.
#'
#' @details
#' If the object provided is multivariate, each row of the result will
#' corresponds to each univariate reward transformation.
#' For \code{dMPH}, \code{qMPH} and \code{pMPH}, the inputs \code{x},
#' \code{p} and \code{q} can be matrices where in row i the i_th reward
#' transformation and in col j the j_th value of \code{x}, \code{p} or \code{q}
#' tested.
#'
#' @import expm
#'
#' @return \code{dMPH} gives the density, \code{pMPH} gives the
#' distribution function, \code{qMPH} gives the quantile function,
#' and \code{rMPH} generates random deviates.
#'
#' The length of the result is determined by \code{n} for \code{rMPH},
#' and is the maximum of the lengths of the numerical arguments for the other
#' functions.
#'
#' The numerical arguments other than \code{n} are recycled to the length of the
#' result. Only the first elements of the logical arguments are used.
#'
#' @seealso
#' \link[stats]{Distributions} for other standard distributions.
#'
#' @examples
#'
#' cont_phase_type <- matrix(c(-3, 0, 1,
#'                             2, -3, 1,
#'                             1, 1, -2), ncol = 3)
#' R <- matrix(c(0, 1, 1,
#'               2, 1, 5,
#'               0, 1, 10), ncol = 3)
#' Y <- MPH(cont_phase_type, reward_mat = R)
#'
#' dMPH(3:4, Y)
#' pMPH(1.45, Y)
#' qMPH(0.5, Y)
#' rMPH(6, Y)
#'
#' @name MPH_functions
NULL
#> NULL


#' @describeIn MPH_functions
#'
#' Density function for the multivariate continuous phase-type distribution.
#'
#' @export

dMPH <- function(x, obj){

  if (class(obj) == 'mult_cont_phase_type') {
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(x))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- dPH(x,reward_phase_type(obj, reward[,i]))
    }
    return(n_mat)
  } else {
    stop("Please provide an object of class 'mult_cont_phase_type'.")
  }
}

#' @describeIn MPH_functions
#'
#' Quantile function for the multivariate continuous phase-type distribution.
#'
#'
#' @import stats
#'
#' @export

qMPH <- function(p, obj){

  if (class(obj) == 'mult_cont_phase_type') {
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(p))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- qPH(p,reward_phase_type(obj, reward[,i]))
    }
    return(n_mat)
  } else {
    stop("Please provide an object of class 'mult_cont_phase_type'.")
  }
}

#' @describeIn MPH_functions
#'
#' Distribution function for the multivariate continuous phase-type distribution.
#'
#'
#' @import stats
#'
#' @export

pMPH <- function(q, obj){

  if (class(obj) == 'mult_cont_phase_type') {
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(q))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- pPH(q,reward_phase_type(obj, reward[,i]))
    }
    return(n_mat)
  } else {
    stop("Please provide an object of class 'mult_cont_phase_type'.")
  }
}

#' @describeIn MPH_functions
#'
#' Random number generator for the multivariate continuous phase-type distribution.
#'
#'
#' @import stats
#'
#' @export

rMPH <- function(n, obj){

  if (length(n) > 1){
    n <- length(n)
  }

  # get the sub-intensity matrix
  subint_mat <- obj$subint_mat
  # get initial probabilities for p+1 states
  init_probs <- c(obj$init_probs, obj$defect)
  # number of states
  p <- nrow(subint_mat)
  # create vector of zeroes
  n_vec <- numeric(n)

  if (class(obj) == 'mult_cont_phase_type') {
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = n)
    for (i in 1:ncol(reward)){
      n_mat[i,] <- rPH(n,reward_phase_type(obj, reward[,i]))
    }
    return(n_mat)
  } else {
    stop("Please provide an object of class 'mult_cont_phase_type'.")
  }
}

