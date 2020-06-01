#' Phase-type simulations
#'
#' sim_phase_type implements a naive simulation scheme for simulation of
#' phase-type distributions.
#'
#' @value A list containing a first vector with the the time in each state and
#' a second vector with the state path before reaching the absorbing state.
#'
#' @param obj an object of class \code{disc_phase_type} or
#' \code{cont_phase_type}
#'
#' @usage sim_phase_type
#'
#' @examples
#'
#' ##===========================##
#' ## For continuous univariate ##
#' ##===========================##
#'
#' set.seed(2)
#' subintensity_matrix <- matrix(c(-1.5, 0, 0.5,
#'                                1.5, -1, 0,
#'                                0, 1, -1), ncol = 3)
#' obj <- phase_type(subintensity_matrix)
#'
#' sim_phase_type(obj)
#'
#' ##===========================##
#' ## For discrete multivariate ##
#' ##===========================##
#'
#' set.seed(2)
#' subintensity_matrix <- matrix(c(0.4, 0, 0.1,
#'                                0.24, 0.6, 0.3,
#'                                0.26, 0.4, 0.5), ncol = 3)
#' obj <- phase_type(subintensity_matrix)
#'
#' sim_phase_type(obj)
#'
#' ##=============================##
#' ## For continuous multivariate ##
#' ##=============================##
#'
#' subintensity_matrix <- matrix(c(-3, 0, 0,
#'                                2, -2, 0,
#'                                0, 1, -1), nrow = 3, ncol = 3)
#' reward_matrix = matrix(sample(seq(0, 10, 0.1), 6), nrow = 3, ncol = 2)
#' phase_type(subintensity_matrix, reward_mat = reward_matrix)
#'
#' ##===========================##
#' ## For discrete multivariate ##
#' ##===========================##
#'
#' subintensity_matrix <- matrix(c(0.4, 0, 0,
#'                                0.24, 0.4, 0,
#'                                0.12, 0.2, 0.5), ncol = 3)
#' reward_matrix <- matrix(sample(seq(0, 10), 6), nrow = 3, ncol = 2)
#' phase_type(subintensity_matrix, reward_mat = reward_matrix)
#'
#' @export

sim_phase_type <- function(obj){
  if (!(class(obj) == 'cont_phase_type') &&
      !(class(obj) == 'disc_phase_type')) {
    stop("Please provide an object of 'class disc_phase_type' or", "
         'cont_phase_type'")
  }

  init_probs <- obj$init_probs
  n <- length(init_probs)

  subint_mat <- obj$subint_mat
  if (class(obj) == ('cont_phase_type')){
    exit_rate <- -subint_mat %*% matrix(1, n, 1)
  } else {
    exit_rate <- 1 - t(t(rowSums(subint_mat)))
  }

  int_mat <- cbind(subint_mat, exit_rate)
  states <- 1:(n+1)
  curstate <- sample(1:n, 1, prob = init_probs) #sample initial state
  states <- curstate

  if(class(obj) == 'cont_phase_type'){
    curtime <- 0 #current time, starts at 0
    times <- 0
    while(curstate <= n){
      curtime <- rexp(1, -subint_mat[curstate, curstate])
      curstate <- sample((1:(n + 1))[-curstate], 1,
                         prob = int_mat[curstate, -curstate])
      times <- c(times, curtime)
      states <- c(states, curstate)
    }
  } else {
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
  }

  return(list(times, states[-length(states)]))
}







##########################################

# mvrewsim multivariate rewards via simulation

# Input: R - number of replications
# reward_mat Rewards matrix p times n - if a vector is provided it is converted to a matrix and univariate rewards are assumed
# the number of columns of reward_mat must match subint_mat

# output: An n times R vector

mvrewsim <- function(R, obj, reward_mat = NULL) {
  if (!(class(obj) == 'mult_cont_phase_type' || class(obj) == 'cont_phase_type')) {
    stop("Please provide an object of type cont_phase_type or mult_cont_phase_type")
  }
  subint_mat <- obj$subint_mat
  if (class(obj) == 'mult_cont_phase_type') {
    reward_mat <- obj$reward_mat
    ph_obj <- phase_type(obj$subint_mat, obj$init_probs)
  }
  if (class(obj) == 'cont_phase_type') {
    if (is.null(reward_mat)) {stop("Please provide a reward matrix")}
    if (is.numeric(reward_mat) & is.null(dim(reward_mat))) {reward_mat <- matrix(reward_mat, length(reward_mat), 1)}
    ph_obj <- obj
  }

  p <- dim(reward_mat)[1]
  n <- dim(reward_mat)[2]
  if (p != dim(subint_mat)[1]) {stop("Dimension mismatch between subint_mat and reward_mat")}
  out <- matrix(0, n, R)
  for(i in 1:R) {
    smph <- naivesimph(ph_obj); #Simulate a single path
    pointwiserewards <- matrix(0, n, length(smph[[1]])) # a vector to hold the rewards oexit_rateained for each state (no reward is oexit_rateained in the aborbing state so length is that of the inter-jump-times)
    # right now, smph[[2]] holds a rowvector of the states visited prior to in incl. absorexit_rateion
    #
    for(j in 1:p) {
      pointwiserewards[, which(smph[[2]] == j)] <- reward_mat[j,]
    }
    #calculate the rewards times the time spent in each state using point-wise multiplication
    out[,i] <- rowSums(pointwiserewards * matrix(smph[[1]], n, length(smph[[1]]), byrow = TRUE))
  }
  return(out)
}
