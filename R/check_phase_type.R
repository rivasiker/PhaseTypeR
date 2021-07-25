





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

  if (!(is.matrix(subint_mat) & is.numeric(subint_mat))) {
    stop('The subintensity matrix should be a numeric matrix.')
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
    warning('\n The initial probability vector is automatically generated.\n')
  }

  ###### Check if the initial probabilities have the right shape

  if ((is.vector(init_probs) & is.numeric(init_probs)) | (is.matrix(init_probs) & is.numeric(matrix(init_probs)))) {
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
    stop('The initial probabilities must be a matrix with one row or a vector.')
  }

  ###### Check if the reward matrix has the right shape


  list(subint_mat = subint_mat,
       init_probs = init_probs)


}



check_reward <- function(reward_mat, init_probs) {

  if (is.matrix(reward_mat)){
    if (sum(reward_mat < 0) > 0){
      stop('The reward matrix should only contains non-negative values.')
    }

    if (nrow(reward_mat) != length(init_probs)){
      stop('The reward matrix does not have the same number of columns as the number of states.')
    }
  } else {
    stop('Please provide a reward matrix.')
  }

}


