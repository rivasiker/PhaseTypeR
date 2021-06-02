#' The Univariate Continuous Phase-Type Distribution
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param obj an object of class \code{cont_phase_type}.
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'  the number required.
#'
#' @details
#' If the object provided is multivariate, each row of the result will
#' corresponds to each univariate reward transformation.
#' For \code{dPH}, \code{qPH} and \code{pPH}, the inputs \code{x},
#' \code{p} and \code{q} can be matrices where in row i the i_th reward
#' transformation and in col j the j_th value of \code{x}, \code{p} or \code{q}
#' tested.
#'
#' @import expm
#'
#' @return \code{dPH} gives the density, \code{pPH} gives the
#' distribution function, \code{qPH} gives the quantile function,
#' and \code{rPH} generates random deviates.
#'
#' The length of the result is determined by \code{n} for \code{rPH},
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
#' Y <- PH(cont_phase_type)
#'
#' dPH(3:4, Y)
#' pPH(1.45, Y)
#' qPH(0.5, Y)
#' rPH(6, Y)
#'
#'
#' @name PH_functions
NULL
#> NULL


#' @describeIn PH_functions
#'
#' Density function for the univariate continuous phase-type distribution.
#'
#' @export


dPH <- function(x, obj){

  if (class(obj) == 'cont_phase_type') {
    vec <- c()
    e <- matrix(rep(1,nrow(obj$subint_mat)), nrow(obj$subint_mat), 1)
    for (i in x) {
      vec <- c(vec, -obj$init_probs %*% expm(i * obj$subint_mat)
               %*% obj$subint_mat %*% e)
    }
    return(vec)
  } else {
    stop("Please provide an object of class 'cont_phase_type'.")
  }
}



#' @describeIn PH_functions
#'
#' Quantile function for the univariate continuous phase-type distribution.
#'
#'
#' @import stats
#'
#' @export

qPH <- function(p, obj){

  vec <- c()
  inv <- function(y) uniroot(function(q) pPH(q, obj)-y, c(0,400))$root[1]
  if (class(obj) == 'cont_phase_type') {
    for (i in p) {
      vec <- c(vec, inv(i))
    }
  } else {
    stop("Please provide an object of class 'cont_phase_type'.")
  }
  return(vec)
}


#' @describeIn PH_functions
#'
#' Distribution function for the univariate continuous phase-type distribution.
#'
#'
#' @import stats
#'
#' @export


pPH <- function(q, obj){

  if (class(obj) == 'cont_phase_type') {
    vec <- c()
    e <- matrix(rep(1,nrow(obj$subint_mat)), nrow(obj$subint_mat), 1)
    for (i in q) {
      vec <- c(vec, 1 - obj$init_probs %*% expm(obj$subint_mat * i) %*% e)
    }
    return(vec)
  } else {
    stop("Please provide an object of class 'cont_phase_type'.")
  }
}



#' @describeIn PH_functions
#'
#' Random number generator for the univariate continuous phase-type distribution.
#'
#'
#' @import stats
#'
#' @export


rPH <- function(n, obj){

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

  if (class(obj) == 'cont_phase_type') {

    # define the intensity matrix by adding the p+1 state column
    int_mat <- cbind(subint_mat, -rowSums(subint_mat))

    # for each n
    for (i in 1:n) {
      # define initial state
      j <- sample(p + 1, 1, prob = init_probs)
      # while the state is not the absorbing state
      while (j != (p + 1)) {
        # update by adding from an exponential draw
        n_vec[i] <- n_vec[i] + rexp(1, -int_mat[j,j])
        # update to the new state
        j <- sample((1:(p + 1))[-j], 1, prob = int_mat[j,-j])
      }
    }
    return(n_vec)

  } else {
    stop("Please provide an object of class 'cont_phase_type'.")
  }
}



