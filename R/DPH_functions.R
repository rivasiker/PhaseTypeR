#' The Univariate Discrete Phase-Type Distribution
#'
#' Density, distribution function, quantile function and random generation
#' for the univariate discrete phase-type distribution.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param obj an object of class \code{disc_phase_type}.
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'  the number required.
#'
#'
#' @import expm
#'
#' @return \code{dDPH} gives the density, \code{pDPH} gives the
#' distribution function, \code{qDPH} gives the quantile function,
#' and \code{rDPH} generates random deviates. \code{rFullDPH} returns
#' the full path of a random draw from the distribution.
#'
#' The length of the result is determined by \code{n} for \code{rDPH},
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
#'
#' disc_phase_type <- matrix(c(0.4, 0, 0.2,
#'                             0.5, 0.3, 0.2,
#'                             0, 0.7, 0.2), ncol = 3)
#' Y <- DPH(disc_phase_type)
#'
#' dDPH(3:4, Y)
#' pDPH(5, Y)
#' qDPH(0.5, Y)
#' set.seed(0)
#' rDPH(6, Y)
#' rFullDPH(Y)
#'
#' @name DPH_functions
NULL
#> NULL


#' @describeIn DPH_functions
#'
#' Density function for the univariate continuous phase-type distribution.
#'
#' @export


dDPH <- function(x, obj){

  if (class(obj) == 'disc_phase_type') {
    if (sum(x %% 1 > 0) > 0){
      stop('x should only contain integers.')
    }
    e <- matrix(1, nrow = nrow(obj$subint_mat))
    t <- e - obj$subint_mat %*% e
    dens_vec <- c()
    for(i in x){
      if (i == 0) {
        dens_vec <- c(dens_vec, obj$defect)
      } else {
        dens_vec <- c(dens_vec, obj$init_probs %*% (obj$subint_mat %^% (i-1))
                      %*% t)
      }
    }
    return(dens_vec)
  } else {
    stop("Please provide an object of class 'disc_phase_type'.")
  }
}


#' @describeIn DPH_functions
#'
#' Quantile function for the univariate discrete phase-type distribution.
#'
#'
#' @import stats
#'
#' @export


qDPH <- function(p, obj){

  vec <- c()
  inv <- function(y) uniroot(function(q) pDPH(q, obj)-y, c(0,400))$root[1]
  if (class(obj) == 'disc_phase_type') {
    for (i in p) {
      vec <- c(vec, round(inv(i)))
    }
  } else {
    stop("Please provide an object of class 'disc_phase_type'.")
  }
  return(vec)
}

#' @describeIn DPH_functions
#'
#' Distribution function for the univariate discrete phase-type distribution.
#'
#'
#' @import stats
#'
#' @export


pDPH <- function(q, obj){

  if (class(obj) == 'disc_phase_type') {
    e <- matrix(1, nrow = nrow(obj$subint_mat))
    prob_vec <- c()
    for(i in q){
      prob_vec <- c(prob_vec, 1 - obj$init_probs %*% (obj$subint_mat %^% i)
                    %*% e)
    }
    return(prob_vec)
  } else {
    stop("Please provide an object of class 'disc_phase_type'.")
  }
}
#' @describeIn DPH_functions
#'
#' Random number generator for the univariate discrete phase-type distribution.
#'
#'
#' @import stats
#'
#' @export


rDPH <- function(n, obj){

  if (class(obj) == 'disc_phase_type') {

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
    int_mat <- cbind(subint_mat, 1 - rowSums(subint_mat))

    # for each n
    for (i in 1:n) {
      # define initial state
      j <- sample(p + 1, 1, prob = init_probs)
      # while the state is not the absorbing state
      while (j != (p + 1)) {
        # update by adding one
        n_vec[i] <- n_vec[i] + 1
        # update to the new state
        j <- sample(p + 1, 1, prob = int_mat[j,])
      }
    }

    return(n_vec)

  } else {
    stop("Please provide an object of class 'disc_phase_type'.")
  }
}


#' @describeIn DPH_functions
#'
#' Simulation of the full path for the univariate discrete phase-type distribution.
#'
#'
#' @import stats
#'
#' @export

rFullDPH <- function(obj){
  if (!(class(obj) == 'disc_phase_type')){
    stop("Please provide an object of class 'disc_phase_type'.")
  }

  init_probs <- obj$init_probs
  n <- length(init_probs)
  subint_mat <- obj$subint_mat

  exit_rate <- 1 - t(t(rowSums(subint_mat)))

  int_mat <- cbind(subint_mat, exit_rate)
  states <- 1:(n+1)
  curstate <- sample(1:n, 1, prob = init_probs) #sample initial state
  states <- curstate
  times <- NULL

  curtime <- 1
  while(curstate <= n){
    curstate <- sample(1:(n+1), 1, prob = int_mat[curstate,])
    if (curstate == states[length(states)]){
      curtime <- curtime + 1
    } else {
      times <- c(times, curtime)
      states <- c(states, curstate)
      curtime <- 1
    }
  }

  return(data.frame(state = states[-length(states)], time = times))
}

