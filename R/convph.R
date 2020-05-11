#' \code{convph} function
#'
#' Description of the class \code{cont_phase_type}, which represents continuous phase-type
#' distributions.
#'
#' \code{convph} is the function allowing convolution between two phase_type object
#' building a new phase_type object.
#'
#' @param ph1 cont_phase_type class
#' @param ph2 cont_phase_type class.
#'
#' @usage convph(ph1 = NULL, ph2 = NULL)
#'
#' @examples
#' ph1 = phase_type(matrix(c(-3, 0, 0, 1, -2, 0, 0, 1, -1), ncol = 3), c(0.25,0.25,0.5))
#' ph2 = phase_type(matrix(c(-1.5, 0, 0, 1.5, -1, 0, 0, 1, -0.5), ncol = 3), c(0.25,0.25,0.5))
#'
#' convph(ph1,ph2)
#' #---
#'
#' @importFrom methods is
#'
#' @export



convph <- function(ph1 = NULL, ph2 = NULL) {
  # Check that both input are of type cont_phase_type
  if(is(ph1, 'cont_phase_type') & is(ph2, 'cont_phase_type')){

    # See theorem 3.1.26 Bladt
    init_probs = c(ph1$init_prob, ph2$init_prob*0)

    exit_rate_ph1 = -apply(ph1$subint_mat,1,sum)

    # See theorem 3.1.26 Bladt
    subint_mat = rbind(cbind(ph1$subint_mat,
                             exit_rate_ph1 %*% ph2$init_probs),
                       cbind(ph2$subint_mat * 0,
                             ph2$subint_mat))

    # Return a new object of type cont_phase_type
    obj = phase_type(subint_mat, init_probs)
    return(obj)

  }else{
    print('Please provide cont_phase_type class object')
  }
}


