

#' @rdname generator_functions
#' @order 4
#'
#' @title Multivariate continuous phase-type distribution
#'
#' @usage MPH(subint_mat = NULL, init_probs = NULL,
#'     reward_mat = NULL)
#'
#' @examples
#'
#'
#' ##=============================##
#' ## For continuous multivariate ##
#' ##=============================##
#'
#' subintensity_matrix <- matrix(c(-3,  2,  0,
#'                                  0, -2,  1,
#'                                  0,  0, -1),
#'                               nrow = 3,
#'                               byrow = TRUE)
#' reward_matrix = matrix(sample(seq(0, 10, 0.1), 6), nrow = 3, ncol = 2)
#' initial_probabilities = c(1, 0, 0)
#' MPH(subintensity_matrix,
#'     initial_probabilities,
#'     reward_matrix)
#'
#' @export


MPH <- function(subint_mat = NULL, init_probs = NULL,
                reward_mat = NULL) {


  lst_check <- check_phase_type(subint_mat, init_probs)

  subint_mat <- lst_check$subint_mat
  init_probs <- lst_check$init_probs

  check_reward(reward_mat, init_probs)

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
                    reward_mat = reward_mat,
                    defect = 1 - sum(init_probs))
      attr(value, "class") <- "mult_cont_phase_type"
      return(value)
    }
  } else {
    stop('All the diagonal values of the sub-intensity matrix should be negative.')
  }
}
