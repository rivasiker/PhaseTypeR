

#' @rdname generator_functions
#' @order 5
#'
#' @title Multivariate discrete phase-type distribution
#'
#' @usage MDPH(subint_mat = NULL, init_probs = NULL,
#'      reward_mat = NULL)
#'
#' @examples
#'
#' ##===========================##
#' ## For discrete multivariate ##
#' ##===========================##
#'
#' subintensity_matrix <- matrix(c(0.4, 0.24, 0.12,
#'                                 0,   0.4,  0.2,
#'                                 0,   0,    0.5),
#'                               ncol = 3,
#'                               byrow = TRUE)
#' reward_matrix <- matrix(sample(seq(0, 10), 6), nrow = 3, ncol = 2)
#' initial_probabilities = c(1, 0, 0)
#' MDPH(subintensity_matrix,
#'      initial_probabilities,
#'      reward_mat = reward_matrix)
#'
#' @export


MDPH <- function(subint_mat = NULL, init_probs = NULL, reward_mat = NULL) {

  lst_check <- check_phase_type(subint_mat, init_probs)

  subint_mat <- lst_check$subint_mat
  init_probs <- lst_check$init_probs

  check_reward(reward_mat, init_probs)

  #############
  # Check the conditions necessary for discrete phase-type distribution
  #############


  if (sum(subint_mat < 0) == 0){

    if (sum(subint_mat > 1) > 0){
      stop('The subintensity matrix should only contain values between 0 and 1.')
    }


    if (sum(rowSums(subint_mat) > 1) > 0){
      stop('The rowsums should be between 0 and 1.')
    }


    if (is.matrix(reward_mat)){
      if (sum(trunc(reward_mat)) != sum(reward_mat)){
        stop('The reward matrix should only contain integers.')
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
