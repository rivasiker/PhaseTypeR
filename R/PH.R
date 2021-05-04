

#' Continuous phase-type distribution
#'
#' @export



PH <- function(subint_mat = NULL, init_probs = NULL, round_zero = NULL) {


  lst_check <- check_phase_type(subint_mat, init_probs,
                                reward_mat = NULL, round_zero)

  subint_mat <- lst_check$subint_mat
  init_probs <- lst_check$init_probs

  #############
  # Check the conditions necessary for continuous phase-type distribution
  #############


  if (length(which(diag(subint_mat) < 0)) == length(init_probs)) {

    # check if any rowsums are positive

    if (any(rowSums(subint_mat)>1e-14)){
      stop('The row sums of the subintensity matrix should be non-positive.')
    } else {
      value <- list(subint_mat = subint_mat,
                    init_probs = init_probs,
                    defect = 1 - sum(init_probs))
      attr(value, "class") <- "cont_phase_type"
      return(value)
    }
  } else {
    stop('All the diagonal values of the sub-intensity matrix should be negative.')
  }
}
