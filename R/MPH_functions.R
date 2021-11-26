#' The Multivariate Continuous Phase-Type Distribution
#'
#' Density, distribution function, quantile function and random generation
#' for the multivariate continuous phase-type distribution.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param obj an object of class \code{mult_cont_phase_type}.
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'  the number required.
#'
#'
#' @import expm
#'
#' @return \code{dMPH} gives the density, \code{pMPH} gives the
#' distribution function, \code{qMPH} gives the quantile function,
#' and \code{rMPH} generates random deviates. \code{rFullMPH} returns
#' the full path of a random draw from the distribution.
#'
#' Each row of the result of For \code{dMPH}, \code{pMPH}, \code{qMPH}, and
#' \code{rMPH} corresponds to each univariate reward transformation.
#' For \code{dMDPH}, \code{qMDPH} and \code{pMDPH}, the inputs \code{x},
#' \code{p} and \code{q} can be matrices where in row i the i_th reward
#' transformation and in col j the j_th value of \code{x}, \code{p} or \code{q}
#' tested.
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
#' R <- matrix(c(0, 1, 1,  2,
#'                  2, 1, 5,  2,
#'                  0, 1, 10, 2), nrow = 3, ncol=4, byrow=TRUE)
#' Y <- MPH(cont_phase_type, reward_mat = R)
#'
#' dMPH(3:4, Y)
#' pMPH(1.45, Y)
#' qMPH(0.5, Y)
#' set.seed(0)
#' rMPH(6, Y)
#' rFullMPH(Y)
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
    x2 <- PH(obj$subint_mat, obj$init_probs)
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(x))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- dPH(x,reward_phase_type(x2, reward[,i]))
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
    x2 <- PH(obj$subint_mat, obj$init_probs)
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(p))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- qPH(p,reward_phase_type(x2, reward[,i]))
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
    x2 <- PH(obj$subint_mat, obj$init_probs)
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(q))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- pPH(q,reward_phase_type(x2, reward[,i]))
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

  if (class(obj) == 'mult_cont_phase_type') {
    x2 <- PH(obj$subint_mat, obj$init_probs)

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


    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = n)
    for (i in 1:ncol(reward)){
      n_mat[i,] <- rPH(n,reward_phase_type(x2, reward[,i]))
    }
    return(n_mat)
  } else {
    stop("Please provide an object of class 'mult_cont_phase_type'.")
  }
}


#' @describeIn MPH_functions
#'
#' Simulation of the full path for the multivariate continuous phase-type distribution.
#'
#'
#' @import stats
#'
#' @export



rFullMPH <- function(obj){
  if (!(class(obj) == 'mult_cont_phase_type')){
    stop("Please provide an object of class 'mult_cont_phase_type'.")
  }

  init_probs <- obj$init_probs
  n <- length(init_probs)
  subint_mat <- obj$subint_mat
  reward_mat <- obj$reward_mat
  p <- ncol(reward_mat)
  out <- matrix(0, n)

  smph <- rFullPH(PH(subint_mat, init_probs))

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




