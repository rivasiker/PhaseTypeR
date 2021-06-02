#' The Multivariate Discrete Phase-Type Distribution
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param obj an object of class \code{mult_disc_phase_type}.
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'  the number required.
#'
#' @details
#' If the object provided is multivariate, each row of the result will
#' corresponds to each univariate reward transformation.
#' For \code{dMDPH}, \code{qMDPH} and \code{pMDPH}, the inputs \code{x},
#' \code{p} and \code{q} can be matrices where in row i the i_th reward
#' transformation and in col j the j_th value of \code{x}, \code{p} or \code{q}
#' tested.
#'
#' @import expm
#'
#' @return \code{dMDPH} gives the density, \code{pMDPH} gives the
#' distribution function, \code{qMDPH} gives the quantile function,
#' and \code{rMDPH} generates random deviates.
#'
#' The length of the result is determined by \code{n} for \code{rMDPH},
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
#' disc_phase_type <- matrix(c(0.4, 0, 0.2,
#'                             0.5, 0.3, 0.2,
#'                             0, 0.7, 0.2), ncol = 3)
#' R <- matrix(c(0, 1, 1,
#'               2, 1, 5,
#'               0, 1, 10), ncol = 3)
#' Y <- MDPH(disc_phase_type, reward_mat = R)
#'
#' dMDPH(3:4, Y)
#' pMDPH(1.45, Y)
#' qMDPH(0.5, Y)
#' rMDPH(6, Y)
#'
#' @name MDPH_functions
NULL
#> NULL



#' @describeIn MDPH_functions
#'
#' Density function for the multivariate discrete phase-type distribution.
#'
#' @export


dMDPH <- function(x, obj){

  if (class(obj) == 'mult_disc_phase_type') {
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(x))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- dDPH(x,reward_phase_type(obj, reward[,i]))
    }
    return(n_mat)
  } else {
    stop("Please provide an object of class 'mult_disc_phase_type'.")
  }
}
#' @describeIn MDPH_functions
#'
#' Quantile function for the multivariate discrete phase-type distribution.
#'
#'
#' @import stats
#'
#' @export

qMDPH <- function(p, obj){

  if (class(obj) == 'mult_disc_phase_type') {
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(p))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- qDPH(p,reward_phase_type(obj, reward[,i]))
    }
    return(n_mat)
  } else {
    stop("Please provide an object of class 'mult_disc_phase_type'.")
  }
}


#' @describeIn MDPH_functions
#'
#' Distribution function for the multivariate discrete phase-type distribution.
#'
#'
#' @import stats
#'
#' @export

pMDPH <- function(q, obj){

  if (class(obj) == 'mult_disc_phase_type') {
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(q))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- pDPH(q,reward_phase_type(obj, reward[,i]))
    }
    return(n_mat)
  } else {
    stop("Please provide an object of class 'mult_disc_phase_type'.")
  }
}

#' @describeIn MDPH_functions
#'
#' Random number generator for the multivariate discrete phase-type distribution.
#'
#'
#' @import stats
#'
#' @export

rMDPH <- function(n, obj){

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

  if (class(obj) == 'mult_disc_phase_type') {
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = n)
    for (i in 1:ncol(reward)){
      n_mat[i,] <- rDPH(n,reward_phase_type(obj, reward[,i]))
    }
    return(n_mat)
  } else {
    stop("Please provide an object of class 'mult_disc_phase_type'.")
  }
}


