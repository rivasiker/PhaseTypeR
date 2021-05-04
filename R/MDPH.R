#' Multivariate discrete phase-type distribution
#'
#' @export


MDPH <- function(subint_mat = NULL, init_probs = NULL,
                 reward_mat = NULL, round_zero = NULL) {

  if (!is.matrix(reward_mat)){
    stop('Please provide a reward matrix.')
  }

  lst_check <- check_phase_type(subint_mat, init_probs,
                                reward_mat = NULL, round_zero)

  subint_mat <- lst_check$subint_mat
  init_probs <- lst_check$init_probs

  #############
  # Check the conditions necessary for discrete phase-type distribution
  #############


  if (sum(subint_mat < 0) == 0){
    if (sum(rowSums(subint_mat > 1)) > 0){
      stop('The rowsums should be between 0 and 1.')
    }

    if (sum(subint_mat > 1) > 0){
      stop('The subintensity matrix should only contain values between
           0 and 1.')
    }
    if (is.matrix(reward_mat)){
      if (sum(trunc(reward_mat)) != sum(reward_mat)){
        stop('The reward matrix should only contains integers.')
      }
      value <- list(subint_mat = subint_mat,
                    init_probs = init_probs,
                    reward_mat = reward_mat,
                    defect = 1 - sum(init_probs))
      attr(value, "class") <- "mult_disc_phase_type"
      return(value)
    }
  } else {
    stop('All of the values in the sub-intensity matrix should be non-negative.')
  }


}
