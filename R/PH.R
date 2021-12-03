
#' @rdname generator_functions
#' @order 2
#'
#' @title Continuous phase-type distribution
#'
#' @usage PH(subint_mat = NULL, init_probs = NULL)
#'
#' @examples
#'
#' ##===========================##
#' ## For continuous univariate ##
#' ##===========================##
#'
#' subintensity_matrix <- matrix(c(-1.5, 1.5, 0,
#'                                  0,  -1,   1,
#'                                  0,   0,  -0.5),
#'                               ncol = 3,
#'                               byrow = TRUE)
#' PH(subintensity_matrix)
#'
#' #---
#'
#' subintensity_matrix <- matrix(c(-1.5, 1.5, 0,
#'                                  0,  -1,   1,
#'                                  0,   0,  -0.5),
#'                               ncol = 3,
#'                               byrow = TRUE)
#' initial_probabilities <- c(0.9, 0.1, 0)
#' PH(subintensity_matrix, initial_probabilities)
#'
#' @export



PH <- function(subint_mat = NULL, init_probs = NULL) {


  lst_check <- check_phase_type(subint_mat, init_probs)

  subint_mat <- lst_check$subint_mat
  init_probs <- lst_check$init_probs

  #############
  # Check the conditions necessary for continuous phase-type distribution
  #############


  if (length(which(diag(subint_mat) < 0)) == length(init_probs)) {

    # check if any rowsums are positive

    if (any((rowSums(subint_mat)-.Machine$double.eps^0.25)>1e-14)){
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
