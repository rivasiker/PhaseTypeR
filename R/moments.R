


#' Moments of the univariate continuous phase-type distribution
#'
#' This function calculates the moments for the phase-type
#' distributions.
#'
#' The order of the moment can be specified by \code{m} for a
#' \code{cont_phase_type} object.
#'
#' @param obj a mult_cont_phase_type.
#' @param m an integer.
#'
#' @usage moment_ph(obj, m)
#'
#'
#' @export


moment_ph <- function(obj, m) {
  e <- matrix(rep(1,nrow(obj$subint_mat)), nrow(obj$subint_mat), 1)
  inv <- solve(obj$subint_mat %^% m)
  moment <- as.numeric((-1) ** m * factorial(m) * obj$init_probs %*% inv %*% e)
  return(moment)
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

#' Moments of the multivariate continuous phase-type distribution
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
