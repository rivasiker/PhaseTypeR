

#' Discrete phase-type distribution
#'
#' @export


DPH <- function(subint_mat = NULL, init_probs = NULL, round_zero = NULL) {


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
    value <- list(subint_mat = subint_mat,
                  init_probs = init_probs,
                  defect = 1 - sum(init_probs))
    attr(value, "class") <- "disc_phase_type"
    return(value)
  } else {
    stop('All of the values in the sub-intensity matrix should be non-negative.')
  }


}
