





check_phase_type <- function(subint_mat, init_probs,
                             reward_mat, round_zero) {

  #############
  # Check the conditions necessary for every phase-type distribution
  #############

  ###### Check if sub-intensity matrix is provided

  if (is.null(subint_mat)) {
    stop('Please provide a valid subintensity matrix.')
  }

  ###### Check if sub-intensity matrix is a matrix

  if (!is.matrix(subint_mat)) {
    stop('The subintensity matrix should be a matrix.')
  } else {
    subint_mat <- matrix(as.numeric(subint_mat), ncol = ncol(subint_mat))
  }

  ###### Check the shape of the sub-intensity matrix

  if (nrow(subint_mat) != ncol(subint_mat)){
    stop('Subintensity matrix should be a square numerical matrix.')
  }

  ###### Check if the initial probabilities are provided

  if (is.null(init_probs)) {
    init_probs <- matrix(c(1, rep(0, nrow(subint_mat) - 1)),
                         1, nrow(subint_mat))
    warning('The initial probability vector is automatically generated.\n')
  }

  ###### Check if the initial probabilities have the right shape

  if ((is.vector(init_probs) & is.atomic(init_probs)) | is.matrix(init_probs)) {
    init_probs <- as.numeric(init_probs)
    init_probs <- matrix(init_probs, nrow = 1)
    if (!is.null(round_zero)){
      if (round(round_zero) == round_zero) { # avoid positive value due to
        #approximation
        init_probs[init_probs > 0] <- trunc(init_probs[init_probs > 0] *
                                              10^round_zero) / 10^round_zero
        subint_mat[subint_mat > 0] <- trunc(subint_mat[subint_mat > 0] *
                                              10^round_zero) / 10^round_zero
        print(trunc(subint_mat[subint_mat > 0],
                    round_zero))
      }
    }

    if (nrow(subint_mat) != length(init_probs)) {
      stop('The length of the initial probability vector does not match the size of the subintensity matrix.')
    }

    if (sum(init_probs) == 0){
      warning('The sum of the inital probability is equal to 0 with a defect of 1.')
    }

    if (sum(init_probs) < 0 || sum(init_probs) > 1){
      stop('The sum of the initial probabilities should be between 0 and 1')
    } else if (sum(init_probs < 0) != 0 || sum(init_probs > 1) != 0) {
      stop('Each initial probability should be between 0 and 1.')
    }

  } else {
    stop('The initial probabilities must be a a matrix with one row or
         a vector.')
  }

  ###### Check if the reward matrix has the right shape

  if (is.matrix(reward_mat)){
    if (sum(reward_mat < 0) < 0){
      stop('The reward matrix should only contains non-negative values.')
    }

    if (nrow(reward_mat) != length(init_probs)){
      stop('The reward matrix does not have the same number of columns as the
           number of states.')
    }
  }

  list(subint_mat = subint_mat,
       init_probs = init_probs)


}

















moment_ph <- function(obj, m) {
  e <- matrix(rep(1,nrow(obj$subint_mat)), nrow(obj$subint_mat), 1)
  inv <- solve(obj$subint_mat %^% m)
  moment <- as.numeric((-1) ** m * factorial(m) * obj$init_probs %*% inv %*% e)
  return(moment)
}

#' @describeIn PH pretty summary of the class.
#'
#' @param object a phase-type object
#' @param ... other arguments passed to methods
#'
#' @export

summary.cont_phase_type <- function(object, ...) {
  cat('\nSubintensity matrix:\n')
  print(object$subint_mat)
  cat('\nInitial probabilities:\n')
  print(object$init_probs)
  cat('\nDefect:\n')
  print(object$defect)
  cat('\nMean: ', mean(object), '\n', sep = '')
  cat('\nVariance: ', var(object), '\n\n', sep = '')
}

#' @export

summary.disc_phase_type <- function(object, ...) {
  cat('\nSubintensity matrix:\n')
  print(object$subint_mat)
  cat('\nInitial probabilities:\n')
  print(object$init_probs)
  cat('\nDefect:\n')
  print(object$defect)
  cat('\nMean: ', mean(object), '\n', sep = '')
  cat('\nVariance: ', var(object), '\n\n', sep = '')
}

#' @export

summary.mult_cont_phase_type <- function(object, ...) {
  cat('\nSubintensity matrix:\n')
  print(object$subint_mat)
  cat('\nReward matrix:\n')
  print(object$reward_mat)
  cat('\nInitial probabilities:\n')
  print(object$init_probs)
  cat('\nDefect:\n')
  print(object$defect)
}

#' @export

summary.mult_disc_phase_type <- function(object, ...) {
  cat('\nSubintensity matrix:\n')
  print(object$subint_mat)
  cat('\nReward matrix:\n')
  print(object$reward_mat)
  cat('\nInitial probabilities:\n')
  print(object$init_probs)
  cat('\nDefect:\n')
  print(object$defect)
}

moment_individual <- function(element, obj){
  solve(-obj$subint_mat) %*% diag(obj$reward_mat[,element])
}

moment_row <- function(row, obj) {
  total <- diag(1, nrow(obj$subint_mat), ncol(obj$subint_mat))
  for (individual in lapply(row, moment_individual, obj = obj)) {
    total <- total %*% individual
  }
  sum(obj$init_probs %*% total)
}

perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) X <- rbind(X, cbind(v[i], perm(v[-i])))
    return(X)
  }
}

#' Moments of the multivariate phase-type distribution
#'
#' This function calculates the moments for the multivariate phase-type
#' distributions.
#'
#' The variables for which the moments are calculated can be specified in
#' the \code{v} vector as indices in the reward matrix of the
#' \code{mult_cont_phase_type} object.
#'
#' @param obj a mult_cont_phase_type.
#' @param v a vector or an integer.
#'
#' @usage moment_mph(obj, v)
#'
#' @export

moment_mph <- function(obj, v) {
  v <- matrix(v)
  X <- perm(v)
  sum(apply(X, 1, moment_row, obj = obj))
}

# require the partitions::parts() function (or write it again)
moment_mdph <- function(obj, v){
  Pv <- partitions::parts(v)
  udrj <- list()
  for (s in 1:v){
  }
}
