
#' @rdname generator_functions
#' @order 3
#'
#' @title Discrete phase-type distribution
#'
#' @usage DPH(subint_mat = NULL, init_probs = NULL)
#'
#' @examples
#'
#' ##=========================##
#' ## For discrete univariate ##
#' ##=========================##
#'
#' subintensity_matrix <- matrix(c(0.4, 0.24, 0.12,
#'                                 0,   0.4,  0.2,
#'                                 0,   0,    0.5),
#'                               ncol = 3,
#'                               byrow = TRUE)
#' DPH(subintensity_matrix)
#'
#' #---
#'
#' subintensity_matrix <- matrix(c(0.4, 0.24, 0.12,
#'                                 0,   0.4,  0.2,
#'                                 0,   0,    0.5),
#'                               ncol = 3,
#'                               byrow = TRUE)
#' initial_probabilities <- c(0.9, 0.1, 0)
#' DPH(subintensity_matrix, initial_probabilities)
#'
#' @export


DPH <- function(subint_mat = NULL, init_probs = NULL) {


  lst_check <- check_phase_type(subint_mat, init_probs)

  subint_mat <- lst_check$subint_mat
  init_probs <- lst_check$init_probs

  #############
  # Check the conditions necessary for discrete phase-type distribution
  #############

  if (sum((subint_mat+.Machine$double.eps^0.25) < 0) == 0){

    if (sum((subint_mat-.Machine$double.eps^0.25) > 1) > 0){
      stop('The subintensity matrix should only contain values between 0 and 1.')
    }

    if (sum((rowSums(subint_mat)-.Machine$double.eps^0.25) > 1) > 0){
      stop('The rowsums should be between 0 and 1.')
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
