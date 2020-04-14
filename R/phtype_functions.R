#' Density, distribution function, quantile function and random generation for
#' phase-type distributions.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param obj an object of class \code{cont_phase_type} or
#' \code{disc_phase_type}.
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'  the number required.
#'
#'
#' @import expm
#'
#' @return \code{dphtype} gives the density, \code{pphtype} gives the
#' distribution function, \code{qphtype} gives the quantile function,
#' and \code{rphtype} generates random deviates.
#'
#' The length of the result is determined by \code{n} for \code{rphtype},
#' and is the maximum of the lengths of the numerical arguments for the other
#' functions.
#'
#' The numerical arguments other than \code{n} are recycled to the length of the
#' result. Only the first elements of the logical arguments are used.
#'
#' @describeIn dphtype
#'
#' Density function.
#'
#' @usage dphtype(x, obj)
#' @usage pphtype(q, obj)
#' @usage qphtype(p, obj)
#' @usage rphtype(n, obj)
#'
#' @seealso
#' \link[stats]{Distributions} for other standard distributions.
#'
#' @examples
#'
#' cont_phase_type <- matrix(c(-3, 0, 1,
#'                             2, -3, 1,
#'                             1, 1, -2), ncol = 3)
#' Y <- phase_type(cont_phase_type)
#' x <- seq(0, 2, length.out = 20)
#' plot(x, dphtype(x, Y))
#'
#' ph = rphtype(20, cont_phase_type)
#'
#' @export,

dphtype <- function(x, obj){
  if (class(obj) == 'cont_phase_type') {
    vec <- c()
    e <- matrix(rep(1,nrow(obj$subint_mat)), nrow(obj$subint_mat), 1)
    for (i in x) {
      vec <- c(vec, -obj$init_probs %*% expm(i * obj$subint_mat)
               %*% obj$subint_mat %*% e)
    }
    return(vec)
  } else if (class(obj) == 'disc_phase_type') {
    if (sum(x %% 1 > 0) > 0){
      stop('x should only contains integer')
    }
    e = matrix(1, nrow = nrow(obj$subint_mat))
    t = e - obj$subint_mat %*% e
    dens_vec = c()
    for(i in x){
      if (i==0) {
        dens_vec <- c(dens_vec, obj$defect)
      } else {
        dens_vec <- c(dens_vec, obj$init_probs %*% (obj$subint_mat %^% (i-1))
                      %*% t)
      }
    }
    return(dens_vec)
  } else {
    stop("Please provide a 'cont_phase_type' or a 'disc_phase_type' class.")
  }
}


#' @describeIn dphtype
#'
#' Quantile Function
#'
#'
#' @import stats
#'
#' @export

qphtype <- function(p, obj){
  vec <- c()
  inv <- function(y) uniroot(function(q) pphtype(q, obj)-y, c(0,400))$root[1]
  if (class(obj) == 'cont_phase_type') {
    for (i in p) {
      vec <- c(vec, inv(i))
    }
  } else if (class(obj) == 'disc_phase_type') {
    for (i in p) {
      vec <- c(vec, round(inv(i)))
    }
  } else {
    stop("Please provide a 'cont_phase_type' or a 'disc_phase_type' class.")
  }
  return(vec)
}



#' @describeIn dphtype
#'
#' Distribution function.
#'
#' @usage pphtype(q, obj)
#'
#' @export


pphtype <- function(q, obj){
  if (class(obj) == 'cont_phase_type') {
    vec <- c()
    e <- matrix(rep(1,nrow(obj$subint_mat)), nrow(obj$subint_mat), 1)
    for (i in q) {
      vec <- c(vec, 1 - obj$init_probs %*% expm(i * obj$subint_mat) %*% e)
    }
    return(vec)
  } else if (class(obj) == 'disc_phase_type') {
    e = matrix(1, nrow = nrow(obj$subint_mat))
    prob_vec = c()
    for(i in q){
      prob_vec <- c(prob_vec, 1 - obj$init_probs %*% (obj$subint_mat %^% i)
                    %*% e)
    }
    return(prob_vec)
  } else {
    stop("Please provide a 'cont_phase_type' or a 'disc_phase_type' class.")
  }
}


#' @describeIn dphtype
#'
#' Random number generator
#'
#' @usage rphtype(n, obj)
#'
#' @export


rphtype <- function(n, obj){
  if (length(n) > 1){
    n <- length(n)
  }

  if (class(obj) == 'cont_phase_type') {

    # get the sub-intensity matrix
    subint_mat = obj$subint_mat
    # define the intensity matrix by adding the p+1 state column
    int_mat <- cbind(subint_mat, -rowSums(subint_mat))
    # get initial probabilities for p+1 states
    init_probs = c(obj$init_probs, obj$defect)
    # number of states
    p <- nrow(subint_mat)
    # create vector of zeroes
    n_vec <- numeric(n)

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

  } else if (class(obj) == 'disc_phase_type') {

    # get the sub-intensity matrix
    subint_mat = obj$subint_mat
    # define the intensity matrix by adding the p+1 state column
    int_mat <- cbind(subint_mat, 1 - rowSums(subint_mat))
    # get initial probabilities for p+1 states
    init_probs = c(obj$init_probs, obj$defect)
    # number of states
    p <- nrow(subint_mat)
    # create vector of zeroes
    n_vec <- numeric(n)

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
    stop("Please provide a 'cont_phase_type' or a 'disc_phase_type' class.")
  }
}



