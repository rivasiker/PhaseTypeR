
#' Phase-type distribution to network
#'
#' This function converts a phase-type distribution into an
#' igraph graph object.
#'
#'
#' @param phase_type an object of class \code{disc_phase_type} or
#' \code{cont_phase_type}
#' @param t NULL or numeric. Sampling time for the continuous phase-type distribution.
#'
#' @usage phase_type_to_network(phase_type, t = NULL)
#'
#' @return An igraph graph object of the phase-type distribution.
#'
#' @examples
#'
#' cont_phase_type <- matrix(c(-3, 0, 1,
#'                             2, -3, 1,
#'                             1, 1, -2), ncol = 3)
#' Y <- PH(cont_phase_type)
#' Y_network <- phase_type_to_network(Y)
#' set.seed(28)
#' plot(Y_network)
#'
#' @importFrom igraph graph_from_data_frame
#'
#' @export




phase_type_to_network <- function(phase_type, t = NULL) {

  if (is(phase_type, 'disc_phase_type')) {

    if (!is.null(t)) {
      warning('Argument t not used')
    }

    mat <- phase_type$subint_mat
    x <- rbind(cbind(mat, 1-rowSums(mat)), c(rep(0, nrow(mat)), 1))
    x <- rbind(c(phase_type$init_probs, 1-sum(phase_type$init_probs)), x)
    x <- cbind(rep(0, nrow(x)), x)
    colnames(x) <- paste0('V', 0:(nrow(x)-1))
    rownames(x) <- colnames(x)
    link_mat <- data.frame(from=rownames(x)[row(x)], to=colnames(x)[col(x)],  weight=c(x))
    link_mat <- link_mat[link_mat$weight != 0,]

    id_mat <-
      data.frame(
        id = paste0('V', 0:(nrow(x)-1)),
        color = c('chartreuse3', rep('gray80', nrow(x)-2), 'red')
      )


  } else if (is(phase_type, 'cont_phase_type')) {



    mat <- phase_type$subint_mat
    x <- rbind(cbind(mat, -rowSums(mat)), c(rep(0, nrow(mat)+1)))

    if (is.numeric(t)) {
      x <- round(expm(x*t), 3)
      if (t == 0) {
        x <- rbind(c(phase_type$init_probs, 1-sum(phase_type$init_probs)), x)
        x <- cbind(rep(0, nrow(x)), x)
        colnames(x) <- paste0('V', 0:(nrow(x)-1))
        rownames(x) <- colnames(x)
        id_mat <-
          data.frame(
            id = paste0('V', 0:(nrow(x)-1)),
            color = c('chartreuse3', rep('gray80', nrow(x)-2), 'tomato')
          )
      } else {
        colnames(x) <- paste0('V', 1:(nrow(x)))
        rownames(x) <- colnames(x)
        id_mat <-
          data.frame(
            id = paste0('V', 1:(nrow(x))),
            color = c(rep('gray80', nrow(x)-1), 'tomato')
          )
      }

    } else if (is.null(t)) {
      x <- rbind(c(phase_type$init_probs, 1-sum(phase_type$init_probs)), x)
      x <- cbind(rep(0, nrow(x)), x)
      colnames(x) <- paste0('V', 0:(nrow(x)-1))
      rownames(x) <- colnames(x)
      id_mat <-
        data.frame(
          id = paste0('V', 0:(nrow(x)-1)),
          color = c('chartreuse3', rep('gray80', nrow(x)-2), 'tomato')
        )

    } else {
      stop('Please provide a numeric time or NULL')
    }

    link_mat <- data.frame(from=rownames(x)[row(x)], to=colnames(x)[col(x)],
                           weight=c(x))
    link_mat <- link_mat[link_mat$weight != 0,]

  } else {
    stop('Please provide a cont_phase_type or a disc_phase_type object.')
  }


  graph_from_data_frame(d=link_mat, vertices=id_mat, directed=TRUE)
}
