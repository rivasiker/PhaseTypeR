#' The Phase-Type Distribution
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param obj an object of class \code{cont_phase_type}, \code{disc_phase_type}
#' , \code{mult_cont_phase_type} or \code{mult_disc_phase_type}.
#' @param n number of observations. If length(n) > 1, the length is taken to be
#'  the number required.
#'
#' @details
#' If the object provided is multivariate, each row of the result will
#' corresponds to each univariate reward transformation.
#' For \code{dphtype}, \code{qphtype} and \code{pphtype}, the inputs \code{x},
#' \code{p} and \code{q} can be matrices where in row i the i_th reward
#' transformation and in col j the j_th value of \code{x}, \code{p} or \code{q}
#' tested.
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
#' plot(x, dphtype(x, Y), ylab = 'pdf')
#'
#' # and for the discrete phase-type distribution
#'
#' disc_phase_type <- matrix(c(0.4, 0, 0.2,
#'                             0.5, 0.3, 0.2,
#'                             0, 0.7, 0.2), ncol = 3)
#' Y <- phase_type(disc_phase_type)
#' x <- seq(0, 20, 1) # Only integer for the x values.
#' plot(x, dphtype(x, Y), ylab = 'pmf')
#'
#' rphtype(20, Y)
#'
#' @name Phase-type
NULL
#> NULL


#' @describeIn Phase-type
#'
#' Density function.
#'
#' @export


dphtype <- function(x, obj){

  if (class(obj) == 'mult_cont_phase_type' ||
      class(obj) == 'mult_disc_phase_type') {
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(x))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- dphtype(x,reward_phase_type(obj, reward[,i]))
    }
    return(n_mat)
  }

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
    stop("Please provide a 'cont_phase_type' or a 'disc_phase_type' class.")
  }
}


#' @describeIn Phase-type
#'
#' Quantile Function
#'
#'
#' @import stats
#'
#' @export

qphtype <- function(p, obj){

  if (class(obj) == 'mult_cont_phase_type' ||
      class(obj) == 'mult_disc_phase_type') {
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(p))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- qphtype(p,reward_phase_type(obj, reward[,i]))
    }
    return(n_mat)
  }

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



#' @describeIn Phase-type
#'
#' Distribution function.
#'
#'
#' @export


pphtype <- function(q, obj){

  if (class(obj) == 'mult_cont_phase_type' ||
      class(obj) == 'mult_disc_phase_type') {
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = length(q))
    for (i in 1:ncol(reward)){
      n_mat[i,] <- pphtype(q,reward_phase_type(obj, reward[,i]))
    }
    return(n_mat)
  }

  if (class(obj) == 'cont_phase_type') {
    vec <- c()
    e <- matrix(rep(1,nrow(obj$subint_mat)), nrow(obj$subint_mat), 1)
    for (i in q) {
      vec <- c(vec, 1 - obj$init_probs %*% expm(obj$subint_mat * i) %*% e)
    }
    return(vec)
  } else if (class(obj) == 'disc_phase_type') {
    e <- matrix(1, nrow = nrow(obj$subint_mat))
    prob_vec <- c()
    for(i in q){
      prob_vec <- c(prob_vec, 1 - obj$init_probs %*% (obj$subint_mat %^% i)
                    %*% e)
    }
    return(prob_vec)
  } else {
    stop("Please provide a 'cont_phase_type' or a 'disc_phase_type' class.")
  }
}


#' @describeIn Phase-type
#'
#' Random number generator
#'
#' @export


rphtype <- function(n, obj){

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

  if (class(obj) == 'mult_cont_phase_type' ||
      class(obj) == 'mult_disc_phase_type') {
    reward <- obj$reward
    n_mat <- matrix(0, nrow = ncol(reward), ncol = n)
    for (i in 1:ncol(reward)){
      n_mat[i,] <- rphtype(n,reward_phase_type(obj, reward[,i]))
    }
    return(n_mat)
  }


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

  } else if (class(obj) == 'disc_phase_type') {

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
    stop("Please provide a 'cont_phase_type' or a 'disc_phase_type' class.")
  }
}



