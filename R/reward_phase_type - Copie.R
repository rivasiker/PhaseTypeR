#' Reward transformation
#'
#' Transform a variable following a phase-type distribution according to a
#' non-negative reward vector.
#'
#' @usage reward_phase_type(phase_type = NULL, init_probs = NULL,
#' subint_mat = NULL, reward_vec)
#'
#' @param phase_type an object of class \code{cont_phase_type} or
#'  \code{disc_phase_type}.
#' @param reward a vector or one row matrix of the same length as the number of
#' states.
#' The vector should contains non negative values and only integer for discrete
#' phase-type class.
#'
#'
#' Either \code{phase_type} or \code{init_probs} and \code{subint_mat} should be
#' filled.
#' if both are filled \code{phase_type} will be used.
#' If there is no init_probs (and no phase_type) the first state will have an
#' initial probability of 1.
#'
#' @examples
#' ##===========================##
#' ## For continuous phase-type ##
#' ##===========================##
#'
#' subint_mat <- matrix(c(-3, 1, 1,
#'                       2, -3, 0,
#'                       1, 1, -3), ncol = 3)
#' init_probs <- c(0.9, 0.1, 0)
#' ph <- phase_type(subint_mat, init_probs)
#' reward <- c(1, 0, 4)
#'
#' reward_phase_type(ph, reward)
#'
#' ##=========================##
#' ## For discrete phase-type ##
#' ##=========================##
#'
#' subint_mat <- matrix(c(0.4, 0, 0,
#'                       0.24, 0.4, 0,
#'                       0.12, 0.2, 0.5), ncol = 3)
#' init_probs <- c(0.9, 0.1, 0)
#' reward <- c(1, 0, 4)
#'
#' reward_phase_type(init_probs = init_probs, subint_mat = subint_mat,
#'                   reward = reward)
#' #---
#'
#' subint_mat <- matrix(c(0.4, 0, 0,
#'                       0.24, 0.4, 0,
#'                       0.12, 0.2, 0.5), ncol = 3)
#' reward <- c(0, 4, 2, 1)
#' init_probs <- c(0.9, 0.1, 0)
#' prob_mat <- matrix(c(0, 0.2, 1,
#'                      0.2, 0, 0,
#'                      0.3, 0, 0,
#'                      0.5, 0.8, 0), ncol = 4)
#'
#' reward_phase_type(init_probs = init_probs, subint_mat = subint_mat,
#'                   reward = reward, prob_mat = prob_mat)

#'
#'
#' @export

reward_phase_type <- function(phase_type = NULL, reward = NULL,
                              init_probs = NULL, subint_mat = NULL,
                              prob_mat = NULL, round_zero = NULL){

    # If init_probs and subint_mat are provided, will
    # determine if continuous or discrete
    if ((is.vector(init_probs) || is.null(init_probs))
        && is.matrix(subint_mat)) {
        try(phase_type <- phase_type(subint_mat, init_probs))
    }

    ##=====================##
    ## Discrete phase-type ##
    ##=====================##

    # If discrete will apply the reward transformation
    # found in the PhD of Navarro (2019)

    if (class(phase_type) == 'disc_phase_type' ||
        class(phase_type) == 'mult_disc_phase_type'){
        init_probs <- phase_type$init_probs
        subint_mat <- phase_type$subint_mat

        if(is.matrix(prob_mat)){
            if (nrow(prob_mat) != nrow(subint_mat)){
                stop('The probability matrix is not of the right size')
            }

            if (ncol(prob_mat) != length(reward)){
                stop('The probability matrix is not of the right size')
            }

            if (! all(rowSums(prob_mat) == 1)){
                stop('The ')
            }

            if (! all(prob_mat >= 0) || ! all(prob_mat <= 1)){
                stop('The ')
            }

            method <- 'prob_mat'

        } else {
            if (length(reward) != length(init_probs)){
                stop('The reward vector has wrong dimensions (should be of the',
                     'same size that the inital probabilities).')
            }

            if (sum(reward < 0) != 0){
                stop('The reward vector should only contains non-negative',
                     'values.')
            }

            if (sum(reward) != sum(round(reward))){
                stop('The reward vector should only contains integer.')
            }
            method <- 'reward'
        }

        #########################
        # vector containing the size modified by the rewards,
        # (i.e. reward = 0, size = 1 ; reward = i > 0, size = reward)
        if (method == 'prob_mat'){
            if (length(which(reward == 0) == 1)){
                reward_zero <- which(reward == 0)
                for (i in which(prob_mat[, reward_zero] > 0 &
                                prob_mat[, reward_zero] < 1)){

                    subint_mat <- rbind(subint_mat, subint_mat[i,])
                    subint_mat <- cbind(subint_mat, subint_mat[,i])

                    subint_mat[,i] <- subint_mat[,i] *
                        (1 - prob_mat[i, reward_zero])

                    subint_mat[,ncol(subint_mat)] <-
                        subint_mat[,ncol(subint_mat)] * prob_mat[i, reward_zero]


                    prob_mat <- rbind(prob_mat,
                                      c(rep(0, reward_zero - 1),
                                        prob_mat[i, reward_zero],
                                        rep(0, ncol(prob_mat) - reward_zero)))

                    prob_mat[i, reward_zero] <- 0

                    init_probs <- c(init_probs,
                                    init_probs[i] *
                                        prob_mat[nrow(prob_mat), reward_zero])

                    init_probs[i] <- init_probs[i] *
                        (1 - prob_mat[nrow(prob_mat), reward_zero])
                }
                prob_mat <- prob_mat / rowSums(prob_mat)
            }
            reward_mod <- apply(t(reward * t(prob_mat > 0)), 1, max)
        } else {
            reward_mod <- reward
            prob_mat <- diag(1, length(init_probs))
        }

        # number of transient states
        nb_states <- length(init_probs)

        # Initialisation of the set of all T_tilde matrices
        # i.e. all the rewarded submatrices to go from i to j
        T_tilde_ij <- rep(list(as.list(1:nb_states)), nb_states)
        reward_weirdo <- reward + as.numeric(reward == 0)
        size <- reward_mod + as.numeric(reward_mod == 0)
        # Building of each T_tilde_ij matrix
        for (i in 1:nb_states) {
            for (j in 1:nb_states) {
                matij <- matrix(0, nrow = size[i], ncol = size[j])
                if (i == j) {
                    matij[-size[i], -1] <- diag(1, size[i] - 1)
                }

                matij[size[i], size[j] - reward_weirdo[which(prob_mat[j,] > 0)] + 1] <-
                    subint_mat[i, j] * prob_mat[j, which(prob_mat[j,]>0)]
                T_tilde_ij[[i]][[j]] <- matij
            }
        }

        abs_pos_p <- c(0, cumsum(size * (as.numeric(reward_mod > 0)))) + 1
        abs_pos_z <- c(0, cumsum(size * (as.numeric(reward_mod == 0)))) + 1

        # vector of state w/ positive-zero reward
        p <- which(reward_mod > 0)
        z <- which(reward_mod == 0)

        if (length(z) > 0){
            T_tilde_states <- list(list(p, p), list(p, z),
                                   list(z, p), list(z, z))

            # list containing T++, T+0, T0+ and T00
            T_tilde <- list('pp' = 0, 'zp' = 0,
                            'pz' = 0, 'zz' = 0)
        } else {
            T_tilde_states <- list(list(p, p))
            T_tilde <- list('pp' = 0)
        }


        count <- 1 # To count the number of pass in the loop
        for (i in T_tilde_states){

            # Will give all the combinations of elements from
            # T++, T+0, T0+ and T00 (respectively each loop iteration)
            combn <- as.matrix(expand.grid(i[[1]],i[[2]]))

            # initialisation of the submatrix
            T_tilde[[count]] <- matrix(0, ncol = sum(size[i[[1]]]),
                                       nrow = sum(size[i[[2]]]))

            # Get the position of each elements
            suppressWarnings(ifelse(all(i[[1]] == p),
                                    pos_row <- abs_pos_p, pos_row <- abs_pos_z))
            suppressWarnings(ifelse(all(i[[2]] == p),
                                    pos_col <- abs_pos_p, pos_col <- abs_pos_z))

            # for each combinations, add the corresponding matrix given by
            # T_tilde_ij
            for (j in 1:nrow(combn)){
                selec_combn <- as.vector(combn[j,])

                numcol <- (pos_row[selec_combn[1]]):
                    (pos_row[selec_combn[1] + 1] - 1)
                numrow <- (pos_col[selec_combn[2]]):
                    (pos_col[selec_combn[2] + 1] - 1)

                T_tilde[[count]][numrow, numcol] <-
                    T_tilde_ij[[selec_combn[2]]][[selec_combn[1]]]
            }
            count <- count + 1
        }

        # Calculation of the new transformed matrix
        # according to ... eqn ...
        init_probs_p <- NULL
        for (i in 1:length(p)) {
            init_probs_p <- c(init_probs_p, init_probs[p[i]],
                              rep(0, size[p[i]] - 1))
        }

        if (length(z) > 0) {
            mat_T <- T_tilde$pp +
                (T_tilde$pz
                 %*% solve(diag(1, ncol(T_tilde$zz)) - T_tilde$zz)
                 %*% T_tilde$zp)
            init_probs_z <- init_probs[z]

            alpha <- init_probs_p +
                (init_probs_z
                 %*% solve(diag(1,ncol(T_tilde$zz)) - T_tilde$zz)
                 %*% T_tilde$zp)
        } else {
            mat_T <- T_tilde$pp
            alpha <- init_probs_p
        }
        obj <- phase_type(mat_T, alpha, round_zero = round_zero)
        return(obj)

        ##=======================##
        ## Continuous phase-type ##
        ##=======================##

        # If continuous, will apply the transformation of
        # Bladt and Nielsen 2017.
    } else if (class(phase_type) == 'cont_phase_type' ||
               class(phase_type) == 'mult_cont_phase_type') {
        init_probs <- phase_type$init_probs
        subint_mat <- phase_type$subint_mat

        if (length(reward) != length(init_probs)) {
            stop('The reward vector has wrong dimensions (should be of the',
                 'same size that the inital probabilities).')
        }

        if (sum(reward < 0) != 0) {
            stop('The reward vector should only contains non-negative values.')
        }

        if (sum(reward) != sum(round(reward))) {
            stop('The reward vector should only contains integer.')
        }

        # Section to get the embended matrix of T (the subintensity matrix)
        Q <- subint_mat * 0
        for (i in 1:nrow(subint_mat)) {
            for (j in 1:ncol(subint_mat)) {
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
            # matrix with the transition from the states with positive rewards
            # to the states with zero reward (p = positive and z = zero)
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
        ti <- -(small_p * diag(subint_mat)[p] / reward[p])

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

        obj <- phase_type(mat_T, alpha, round_zero = round_zero)
        return(obj)
    } else {
        stop('The object(s) provided describe neither a continuous neither a',
             'discrete phase-type distribution.')
    }
}

