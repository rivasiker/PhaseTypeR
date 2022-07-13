#' The Univariate Continuous Phase-Type Distribution
#'
#' Density, distribution function, quantile function and random generation
#' for the univariate continuous phase-type distribution.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param obj an object of class \code{cont_phase_type}.
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'  the number required.
#'
#'
#' @import expm
#'
#' @return \code{dPH} gives the density, \code{pPH} gives the
#' distribution function, \code{qPH} gives the quantile function,
#' and \code{rPH} generates random deviates. \code{rFullPH} returns
#' the full path of a random draw from the distribution.
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
#' set.seed(0)
#' rPH(6, Y)
#' rFullPH(Y)
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

  if (is(obj, 'cont_phase_type')) {
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
  if (is(obj, 'cont_phase_type')) {
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

  if (is(obj, 'cont_phase_type')) {
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

  if (is(obj, 'cont_phase_type')) {

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
        j <- ifelse(
          length((1:(p + 1))[-j]) == 1,
          (1:(p + 1))[-j],
          sample((1:(p + 1))[-j], 1, prob = int_mat[j,-j]))
      }
    }
    return(n_vec)

  } else {
    stop("Please provide an object of class 'cont_phase_type'.")
  }
}



#' @describeIn PH_functions
#'
#' Simulation of the full path for the univariate continuous phase-type distribution.
#'
#'
#' @import stats
#'
#' @export

rFullPH <- function(obj){
  if (!(is(obj, 'cont_phase_type'))){
    stop("Please provide an object of class 'cont_phase_type'.")
  }

  init_probs <- obj$init_probs
  n <- length(init_probs)
  subint_mat <- obj$subint_mat

  exit_rate <- -subint_mat %*% matrix(1, n, 1)

  int_mat <- cbind(subint_mat, exit_rate)
  states <- 1:(n+1)
  curstate <- sample(1:n, 1, prob = init_probs) #sample initial state
  states <- curstate
  times <- NULL

  if (is(obj, 'cont_phase_type')){
    while(curstate <= n){
      curtime <- rexp(1, -subint_mat[curstate, curstate])
      curstate <- sample((1:(n + 1))[-curstate], 1,
                         prob = int_mat[curstate, -curstate])
      times <- c(times, curtime)
      states <- c(states, curstate)
    }
  }

  return(data.frame(state = states[-length(states)], time = times))
}




