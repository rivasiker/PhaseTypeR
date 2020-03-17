#' \code{disc_phase_type} class
#'
#' Description of the class \code{disc_phase_type}, which represents discrete
#' phase-type distributions.
#'
#' \code{reward_DPH} is the function to attribute a non-negative
#' reward vector to a discrete phase-type distribution.
#'
#' @param phase_type a disc_phase_type object or \code{NULL} (default).
#' @param init_probs vector, a one-row matrix or \code{NULL} (default).
#' @param subint_mat matrix or \code{NULL} (default).
#' @param reward vector, a one-row matrix or \code{NULL} (default).
#'
#' @usage reward_DPH(DPH = NULL, init_probs = NULL, subint_mat = NULL,
#' reward = NULL)
#'
#' @examples
#' 
#' subint_mat = matrix(c(0.4, 0, 0, 0.24, 0.4, 0, 0.12, 0.2, 0.5), ncol = 3)
#' init_probs = c(0.9, 0.1, 0)
#' reward = c(1,0,4)
#' 
#' disc_reward(init_probs = init_probs,
#'                 subint_mat = subint_mat, reward = reward)
#' ------------------------
#' 
#' 
#'
#' @export


disc_reward <- function(phase_type = NULL, init_probs = NULL,
                       subint_mat = NULL, reward = NULL){
    
    #  Check if the user provide a DPH object
    if (class(phase_type) == 'disc_phase_type'){  
        init_probs <- phase_type$init_probs
        subint_mat <- phase_type$subint_mat
    }
    
    # Check that the user provide information of the right format
    # will not check that the object respect the DPH format
    
    if (is.vector(init_probs) && is.matrix(subint_mat)) {
        
        if (length(init_probs) == ncol(subint_mat) 
            && ncol(subint_mat) == nrow(subint_mat)) {
            
            # number of transient states
            nb_states <- length(init_probs) 
            
            # Initialisation of the set of all T_tilde matrices
            # i.e. all the rewarded submatrices to go from i to j 
            T_tilde_ij <- rep(list(as.list(1:nb_states)), nb_states) 
            
            # vector containing the size modified by the rewards,
            # (i.e. reward = 0, size = 1 ; reward = i > 0, size = reward)
            size <- reward + as.numeric(reward == 0) 
            
            # Building of each T_tilde_ij matrix
            for (i in 1:nb_states) {
                for (j in 1:nb_states) {
                    matij <- matrix(0, nrow = size[i], ncol = size[j])
                    matij[size[i], 1] <- subint_mat[i, j]
                    
                    if(i == j) {
                        matij[-size[i], -1] <- diag(1, size[i] - 1)
                        }
                    
                    T_tilde_ij[[i]][[j]] <- matij
                }
            }
            
            # function necessary to have the right position in the order T_tilde
            # will creates a vector which sum the previous elmts of the vector
            # (e.g. 1 3 0 4 will be 1 4 4 8)
            sumvec <- function(vec) {
                for (i in 2:length(vec)){
                    vec[i] <- vec[i - 1] + vec[i]
                }
                return(vec)
            }
            
            abs_pos_p <- c(0, sumvec(size * (as.numeric(reward > 0)))) + 1
            abs_pos_z <- c(0, sumvec(size * (as.numeric(reward == 0)))) + 1
            
            # vector of state w/ positive-zero reward
            p <- which(reward > 0)
            z <- which(reward == 0)
            T_tilde_states <- list(list(p, p), list(p, z),
                                   list(z, p), list(z, z))
            
            # list containing T++, T+0, T0+ and T00
            T_tilde <- list('pp' = 0, 'zp' = 0,
                            'pz' = 0, 'zz' = 0) 
            
            count <- 1 # To count the number of pass in the loop
            for (i in T_tilde_states){
                
                # Will give all the combinations of elements from 
                # T++, T+0, T0+ and T00 (respectively each loop iteration)
                combn <- as.matrix(expand.grid(i[[1]],i[[2]]))
                
                # initialisation of the submatrix
                T_tilde[[count]] <- matrix(0, ncol = sum(size[i[[1]]]),
                               nrow = sum(size[i[[2]]]))
                
                # Get the position of each elements
                ifelse(i[[1]] == p, pos_row <- abs_pos_p, pos_row <- abs_pos_z)
                ifelse(i[[2]] == p, pos_col <- abs_pos_p, pos_col <- abs_pos_z)
                
                # for each combinations, add the corresponding matrix given by
                # T_tilde_ij
                for (j in 1:nrow(combn)){
                    
                    selec_combn <- as.vector(combn[j,])

                    numcol <- (pos_row[selec_combn[1]]):
                        (pos_row[selec_combn[1] + 1] - 1)
                    numrow <- (pos_col[selec_combn[2]]):
                        (pos_col[selec_combn[2] + 1] - 1)
                    
                    T_tilde[[count]][numrow, numcol] <- T_tilde_ij[[selec_combn[2]]][[selec_combn[1]]]
                }
                count <- count + 1
            }
            
            # Calculation of the new transformed matrix
            # according to ... eqn ...
            mat_T <- T_tilde$pp + 
                (T_tilde$pz 
                 %*% solve(diag(1, ncol(T_tilde$zz)) - T_tilde$zz)
                 %*% T_tilde$zp)
            
            init_probs_p <- NULL
            for (i in 1:length(p)){
                init_probs_p <- c(init_probs_p, init_probs[p[i]],
                                  rep(0, reward[p[i]] - 1))
            }
            
            init_probs_z <- init_probs[z]
            
            alpha <- init_probs_p +
                (init_probs_z 
                 %*% solve(diag(1,ncol(T_tilde$zz)) - T_tilde$zz)
                 %*% T_tilde$zp)
            
            obj <- disc_phase_type(mat_T, alpha)
            return(obj)
            
        }else{
            stop('Wrong dimensions')
            }
    }else{
        stop('Wrong type of object')
        } 
}

