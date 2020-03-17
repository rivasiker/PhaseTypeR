#' \code{disc_phase_type} class
#'
#' Description of the class \code{disc_phase_type}, which represents
#' discrete phase-type distributions.
#'
#' \code{reward_PH} is the function to attribute a non-negative
#' reward vector to a continuous phase-type distribution.
#'
#' @param PH a cont_phase_type object or \code{NULL}, add a PH will 
#' prevail on adding a init_probs and subint_mat (default). 
#' @param init_probs vector, a one-row matrix or \code{NULL} (default).
#' @param subint_mat matrix or \code{NULL} (default).
#' @param reward vector, a one-row matrix or \code{NULL} (default).
#'
#' @usage reward_PH(PH = NULL, init_probs = NULL, subint_mat = NULL,
#' reward_vec = NULL)
#'
#' @examples
#' 
#' Example of bladt 
#' 
#' subint_mat <- matrix(c(-3, 2, 0, 1, -2, 1, 1, 1, -2), nrow = 3, byrow = T)
#' init_probs <- c(0.5, 0.25, 0.25)
#' reward <- c(1, 0, 1)
#' 
#' cont_reward(init_probs = init_probs, subint_mat = subint_mat, reward = reward)
#' ------------------------
#' 
#' @export

cont_reward <- function(PH = NULL, init_probs = NULL,
                     subint_mat = NULL, reward = NULL){
    
    if (class(PH) == 'cont_phase_type'){
        init_probs <- PH$init_probs
        subint_mat <- PH$subint_mat
    }
    
    if (is.matrix(reward)){
        reward = as.vector(reward)
    }
    
    # Section to get the embended matrix of T (the subintensity matrix)
    Q <- subint_mat * 0
    for (i in 1:nrow(subint_mat)){
        for (j in 1:ncol(subint_mat)){
            if (i == j) {
                Q[i, i] <- 0
            } else {
                Q[i,j] <- -(subint_mat[i,j] / subint_mat[i,i]) 
            }
        }
    }    
    
    p <- which(reward > 0)
    z <- which(reward == 0)
    
    if ((length(z) > 0) && (length(p) > 0)){
        
        # Block partionning of Q, with the submatrix Qpz corresponds to the 
        # matrix with the transition from the states with positive rewards to
        # the states with zero reward (p = positive and z = zero)
        Qpp <- matrix(Q[p,p], nrow = length(p))
        Qpz <- matrix(Q[p,z], nrow = length(p))
        Qzp <- matrix(Q[z,p], nrow = length(z))
        Qzz <- matrix(Q[z,z], nrow = length(z))
        
        P <- Qpp + (Qpz %*% solve(diag(1, ncol(Qzz)) - Qzz) %*% Qzp)
        
        alpha <-  init_probs[p] +
            init_probs[z] %*% (solve(diag(1, ncol(Qzz)) - Qzz) %*% Qzp)
        
    } else if ((length(z) == 0) && (length(p) > 0)) {
        # if there is no zero reward, no need to remove them
        P <- Q
        alpha <- init_probs
    } else {
        print('no reward is positive')
    }
    
    # vec_e is a vector of 1 of the same size that P
    vec_e <- rep(1,nrow(P))
    # small_p is the exit rate of P
    small_p <- as.vector(vec_e - P %*% vec_e)
    # ti is the exit rate of the new subintensity matrix (rewarded)
    ti <- -(small_p * reward[p] * diag(subint_mat)[p])
    
    # Initialisation of the new subintensity matrix (rewarded) 
    mat_T <- P * 0
    for (i in 1:nrow(P)){
        for (j in 1:ncol(P)){
            if (i != j){
                mat_T[i,j] <- -(subint_mat[p[i],p[i]]/reward[p[i]])*P[i,j] 
            }
        }
    }
    
    # Calculate the rate of leaving each state
    mat_T <- mat_T - diag((apply(mat_T, 1, sum) + ti))
    
    # Get a cont_phase_type object
    obj <- cont_phase_type(mat_T, alpha)
    return(obj)
}
