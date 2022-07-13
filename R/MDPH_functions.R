#' The Multivariate Discrete Phase-Type Distribution
#'
#' Density, distribution function, quantile function and random generation
#' for the multivariate discrete phase-type distribution.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param obj an object of class \code{mult_disc_phase_type}.
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'  the number required.
#'
#'
#' @import expm
#'
#' @return \code{dMDPH} gives the density, \code{pMDPH} gives the
#' distribution function, \code{qMDPH} gives the quantile function,
#' and \code{rMDPH} generates random deviates. \code{rFullMDPH} returns
#' the full path of a random draw from the distribution.
#'
#' Each row of the result of For \code{dMDPH}, \code{pMDPH}, \code{qMDPH}, and
#' \code{rMDPH} corresponds to each univariate reward transformation.
#' For \code{dMDPH}, \code{qMDPH} and \code{pMDPH}, the inputs \code{x},
#' \code{p} and \code{q} can be matrices where in row i the i_th reward
#' transformation and in col j the j_th value of \code{x}, \code{p} or \code{q}
#' tested.
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
#'               0, 1, 10,
#'               1, 2, 3), nrow = 3)
#' Y <- MDPH(disc_phase_type, reward_mat = R)
#'
#' dMDPH(3:4, Y)
#' pMDPH(1.45, Y)
#' qMDPH(0.5, Y)
#' set.seed(0)
#' rMDPH(6, Y)
#' rFullMDPH(Y)
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

  if (is(obj, 'mult_disc_phase_type')) {
    x2 <- DPH(obj$subint_mat, obj$init_probs)
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(x))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- dDPH(x,reward_phase_type(x2, reward[,i]))
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

  if (is(obj, 'mult_disc_phase_type')) {
    x2 <- DPH(obj$subint_mat, obj$init_probs)
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(p))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- qDPH(p,reward_phase_type(x2, reward[,i]))
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

  if (is(obj, 'mult_disc_phase_type')) {
    x2 <- DPH(obj$subint_mat, obj$init_probs)
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(q))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- pDPH(q,reward_phase_type(x2, reward[,i]))
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

  if (is(obj, 'mult_disc_phase_type')) {

    if (length(n) > 1){
      n <- length(n)
    }
    n_mat <- matrix(0, ncol = ncol(obj$reward_mat), nrow = n)
    for (i in 1:n){
      n_mat[i,] <- colSums(rFullMDPH(obj))[3:(ncol(obj$reward_mat)+2)]
    }
    return(n_mat)
  } else {
    stop("Please provide an object of class 'mult_disc_phase_type'.")
  }
}

#' @describeIn MDPH_functions
#'
#' Simulation of the full path for the multivariate discrete phase-type distribution.
#'
#'
#' @import stats
#'
#' @export



rFullMDPH <- function(obj){
  if (!(is(obj, 'mult_disc_phase_type'))){
    stop("Please provide an object of class 'mult_disc_phase_type'.")
  }
  init_probs <- obj$init_probs
  n <- length(init_probs)
  subint_mat <- obj$subint_mat
  reward_mat <- obj$reward_mat
  p <- ncol(reward_mat)
  out <- matrix(0, n)

  smph <- rFullDPH(DPH(subint_mat, init_probs))

  out <- matrix(0, nrow(smph), p)

  for(j in 1:nrow(smph)) {
    out[j,] <- smph$time[j]*reward_mat[smph$state[j], ]
  }
  #calculate the rewards times the time spent in each state using point-wise multiplication
  out <- as.data.frame(out)
  colnames(out) <- paste0('reward_', 1:p)
  out$state <- smph$state
  out$time <- smph$time

  out <- out[, c(p+1, p+2, 1:p)]

  return(out)

}
