#' Transformation of Phase-Type Distributions via Rewards
#'
#' Transform a variable following a phase-type distribution according to a
#' non-negative reward vector.
#'
#' @usage reward_phase_type(phase_type, reward)
#'
#' @param phase_type an object of class \code{cont_phase_type} or
#'  \code{disc_phase_type}.
#'
#' @param reward a vector of the same length as the number of
#' states. The vector should contain non-negative values. Rewards for the
#' discrete phase-type distribution can only be integers.
#'
#' @return
#' An object of class \code{disc_phase_type} or \code{cont_phase_type}.
#'
#' @details
#' For the reward transformation for continuous phase-type distribution, the
#' transformation will be performed as presented in the book of Bladt and
#' Nielsen (2017).
#'
#' For the discrete phase_type distribution is based on the PhD of Navarro (2018) and
#' Hobolth, Bladt and Andersen (2021).
#'
#'
#'
#' @references
#' Bladt, M., & Nielsen, B. F. (2017). *Matrix-exponential distributions in applied probability* (Vol. 81). New York: Springer.
#'
#' Campillo Navarro, A. (2018). *Order statistics and multivariate discrete phase-type distributions*. DTU Compute. DTU Compute PHD-2018, Vol.. 492
#'
#' Hobolth, A., Bladt, M. & Andersen, L.A. (2021). *Multivariate phase-type theory for the site frequency spectrum*. ArXiv.
#'
#' @seealso \code{\link{PH}}, \code{\link{DPH}}
#'
#' @examples
#' ##===========================##
#' ## For continuous phase-type ##
#' ##===========================##
#'
#' subint_mat <- matrix(c(-3, 1, 1,
#'                       2, -3, 0,
#'                       1, 1, -3), ncol = 3)
#' init_probs <- c(0.9, 0.1, 0)
#' ph <- PH(subint_mat, init_probs)
#' reward <- c(0.5, 0, 4)
#'
#' reward_phase_type(ph, reward)
#'
#' ##=========================##
#' ## For discrete phase-type ##
#' ##=========================##
#'
#' subint_mat <- matrix(c(0.4, 0, 0,
#'                       0.24, 0.4, 0,
#'                       0.12, 0.2, 0.5), ncol = 3)
#' init_probs <- c(0.9, 0.1, 0)
#' ph <- DPH(subint_mat, init_probs)
#'
#' reward <- c(1, 0, 4)
#'
#' reward_phase_type(ph, reward)
#'
#'
#' @export

reward_phase_type <- function(phase_type, reward){

  if (!is.numeric(reward)) {
    stop('Please provide a valid reward vector.')
  }

  ##=====================##
  ## Discrete phase-type ##
  ##=====================##

  # If discrete will apply the reward transformation
  # found in the PhD of Navarro (2018) Section 5.2.12 page 74-78.

  if (class(phase_type) == 'disc_phase_type'){

    init_probs <- phase_type$init_probs
    subint_mat <- phase_type$subint_mat

    n <- length(init_probs)

    if (is.matrix(reward)) {
      if (nrow(reward) == 1){
        reward  <- as.vector(reward)
      } else {
        stop('The reward vector should be a vector or a ',
             'matrix with 1 row.')
      }
    }
    if (is.vector(reward)) {
      if (length(reward) != n){
        stop('The reward vector has wrong dimensions (should be of the ',
             'same size that the inital probabilities).')
      }

      if (sum(reward < 0) != 0){
        stop('The reward vector should only contain non-negative values.')
      }

      if (sum(reward) != sum(round(reward))){
        stop('The reward vector should only contain integers.')
      }

      reward_max <- reward
      reward <- matrix(0, ncol = max(reward) + 1,
                       nrow = n)
      for (i in 1:n){
        reward[i, reward_max[i] + 1] <-  1
      }
    }

    # Initialization of the set of all T_tilde matrices
    # i.e. all the rewarded sub-matrices to go from i to j
    T_tilde_ij <- rep(list(as.list(1:n)), n)
    size <- reward_max + as.numeric(reward_max == 0)
    # Building of each T_tilde_ij matrix
    for (i in 1:n) {
      for (j in 1:n) {
        matij <- matrix(0, nrow = size[i], ncol = size[j])
        if (i == j) {
          matij[-size[i], -1] <- diag(1, size[i] - 1)
        }
        if (sum(reward[j, 2:(max(reward_max)+1)]) > 0){
          matij[size[i], 1:size[j]] <- subint_mat[i, j] *
            reward[j, (size[j] + 1):2]
        } else {
          matij[size[i], 1] <- subint_mat[i, j]
        }

        T_tilde_ij[[i]][[j]] <- matij
      }
    }

    abs_pos_p <- c(0, cumsum(size * (as.numeric(reward_max > 0)))) + 1
    abs_pos_z <- c(0, cumsum(size * (as.numeric(reward_max == 0)))) + 1

    # vector of state w/ positive-zero reward
    p <- which(reward_max > 0)
    z <- which(reward_max == 0)

    if (length(z) > 0){
      T_tilde_states <- list(list(p, p), list(p, z), list(z, p), list(z, z))

      # list containing T++, T+0, T0+ and T00
      T_tilde <- list('pp' = 0, 'zp' = 0, 'pz' = 0, 'zz' = 0)
    } else {
      T_tilde_states <- list(list(p, p))
      T_tilde <- list('pp' = 0)
    }

    count <- 1 # To count the number of pass in the loop
    for (i in T_tilde_states){

      # Will give all the combinations of elements from
      # T++, T+0, T0+ and T00 (respectively each loop iteration)
      combn <- as.matrix(expand.grid(i[[1]],i[[2]]))

      # initialization of the sub-matrix
      T_tilde[[count]] <- matrix(0, ncol = sum(size[i[[1]]]),
                                 nrow = sum(size[i[[2]]]))

      # Get the position of each elements
      suppressWarnings(ifelse(all(i[[1]] == p),
                              pos_row <- abs_pos_p, pos_row <- abs_pos_z))
      suppressWarnings(ifelse(all(i[[2]] == p),
                              pos_col <- abs_pos_p, pos_col <- abs_pos_z))

      # for each combinations, add the corresponding matrix given by
      # T_tilde_ij
      for (j in 1:nrow(combn)){
        selec_combn <- as.vector(combn[j,])

        numcol <- (pos_row[selec_combn[1]]):(pos_row[selec_combn[1] + 1] - 1)
        numrow <- (pos_col[selec_combn[2]]):(pos_col[selec_combn[2] + 1] - 1)

        T_tilde[[count]][numrow, numcol] <-
          T_tilde_ij[[selec_combn[2]]][[selec_combn[1]]]
      }
      count <- count + 1
    }

    # Calculation of the new transformed matrix
    # according to ... eqn ...
    init_probs_p <- NULL
    for (i in 1:length(p)) {
      init_probs_p <- c(init_probs_p, init_probs[p[i]], rep(0, size[p[i]] - 1))
    }

    if (length(z) > 0) {
      subint_mat <- T_tilde$pp + (T_tilde$pz %*% solve(diag(1, ncol(T_tilde$zz))
                                                       - T_tilde$zz) %*% T_tilde$zp)

      init_probs_z <- init_probs[z]
      init_probs <- init_probs_p +
        (init_probs_z %*% solve(diag(1,ncol(T_tilde$zz)) - T_tilde$zz)  %*%
           T_tilde$zp)

    } else {
      subint_mat <- T_tilde$pp
      init_probs <- init_probs_p
    }
    ph <- DPH(subint_mat, init_probs)
    return(ph)

    ##=======================##
    ## Continuous phase-type ##
    ##=======================##

    # If continuous, will apply the transformation of
    # Bladt and Nielsen 2017.
  } else if (class(phase_type) == 'cont_phase_type') {

    init_probs <- phase_type$init_probs
    subint_mat <- phase_type$subint_mat

    n <- length(init_probs)

    if (is.matrix(reward)){
      if (nrow(reward) == 1){
        reward <- as.vector(reward)
      } else {
        stop('The rewards should be a vector.')
      }
    }

    if (length(reward) != n) {
      stop('The reward vector has wrong dimensions (should be of the ',
           'same size that the inital probabilities).')
    }

    if (sum(reward < 0) != 0) {
      stop('The reward vector should only contain non-negative values.')
    }


    # Section to get the embedded matrix of T (the sub-intensity matrix)
    Q <- subint_mat * 0
    for (i in 1:n) {
      for (j in 1:n) {
        if (i == j) {
          Q[i, i] <- 0
        } else {
          Q[i,j] <- -(subint_mat[i,j] / subint_mat[i,i])
        }
      }
    }

    p <- which(reward > 0)
    z <- which(reward == 0)

    if ((length(z) > 0) && (length(p) > 0)){

      # Block partionning of Q, with the sub-matrix Qpz corresponds to the
      # matrix with the transition from the states with positive rewards
      # to the states with zero reward (p = positive and z = zero)
      Qpp <- matrix(Q[p,p], nrow = length(p))
      Qpz <- matrix(Q[p,z], nrow = length(p))
      Qzp <- matrix(Q[z,p], nrow = length(z))
      Qzz <- matrix(Q[z,z], nrow = length(z))

      P <- Qpp + (Qpz %*% solve(diag(1, ncol(Qzz)) - Qzz) %*% Qzp)

      init_probs <-  init_probs[p] +
        init_probs[z] %*% (solve(diag(1, ncol(Qzz)) - Qzz) %*% Qzp)

    } else if ((length(z) == 0) && (length(p) > 0)) {
      # if there is no zero reward, no need to remove them
      P <- Q

    } else {
      stop('None of the rewards are positive.')
    }

    # vec_e is a vector of 1 of the same size that P
    vec_e <- rep(1,nrow(P))
    # small_p is the exit rate of P
    small_p <- as.vector(vec_e - P %*% vec_e)
    # ti is the exit rate of the new sub-intensity matrix (rewarded)
    ti <- -(small_p * diag(subint_mat)[p] / reward[p])

    # initialization of the new sub-intensity matrix (rewarded)
    mat_T <- P * 0
    for (i in 1:nrow(P)){
      for (j in 1:ncol(P)){
        if (i != j){
          mat_T[i, j] <- -(subint_mat[p[i],p[i]]/reward[p[i]])*P[i,j]
        }
      }
    }

    if (length(ti) != 1) {
      diag_mat <- diag((apply(mat_T, 1, sum) + ti))
    } else {
      diag_mat <- matrix((apply(mat_T, 1, sum) + ti))
    }

    # Calculate the rate of leaving each state
    subint_mat <- mat_T - diag_mat
    # Get a cont_phase_type object
    ph <- PH(subint_mat, init_probs)
    return(ph)
  } else {
    stop("The object provided should be of class 'disc_phase_type' or ",
         "'cont_phase_type'.")
  }
}
