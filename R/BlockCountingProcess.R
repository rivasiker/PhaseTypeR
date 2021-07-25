#' Rate matrix and state space of the block counting process
#'
#'
#' \code{RateMatAndStateSpace} finds the state space and corresponding rate matrix
#' for the block counting process for a number of samples n in the
#' standard coalescent.
#'
#' @param n Number of samples
#'
#' @return List consisting of
#' RateM: Rate matrix
#' StSpM: Matrix with rows corresponding to the states
#'        A state is a n-dimensional row vector (a1,...,an).
#'        For example the beginning state is (n,0,...,0),
#'        the next state is (n-2,1,0,...,0),
#'        and the ending state is (0,...,0,1)
#'
#' @examples
#' RateMAndStateSpace(8)
#'
#'
#'
#' @export
RateMAndStateSpace <- function(n){
  ##----------------------------------------------------
  ## State space
  ##----------------------------------------------------
  ## Size of the state space (number of states)
  nSt <- partitions::P(n)
  ## Definition of the state space
  StSpM <- matrix(ncol=n,nrow=nSt)
  ## Set of partitions of [n]
  x <- partitions::parts(n)
  ## Rewriting the partitions as (a1,...,an)
  for (i in 1:nSt) {
    st <- x[,i]
    StSpM[i,] <- tabulate(x[,i],nbins=n)
  }
  ## Reordering
  StSpM <- StSpM[order(rowSums(StSpM),decreasing=TRUE),]
  ## Because of this ordering we can't 'go back', i.e.
  ## below the diagonal the entries are always zero
  ##----------------------------------------------------
  ## Intensity matrix
  ##----------------------------------------------------
  RateM <- matrix(0,ncol=nSt,nrow=nSt)
  ## Following Algorithm 4.2
  for (i in 1:(nSt-1)){
    for (j in (i+1):nSt){
      # cat(i," state i",StSpM[i,])
      # cat(" ",j," state j",StSpM[j,])
      cvec <- StSpM[i,]-StSpM[j,]
      # cat(" cvec",cvec)
      ## Two branches are merged, i.e. removed from state i
      check1 <- sum(cvec[cvec>0])==2
      # cat(" check1",check1)
      ## One new branch is created, i.e. added in state from j
      check2 <- sum(cvec[cvec<0])==-1
      # cat(" check2",check2)
      if (check1 & check2){
        ## Size(s) of the block(s) and the corresponding rates
        tmp <- StSpM[i,which(cvec>0)]
        RateM[i,j] <- ifelse(length(tmp)==1,tmp*(tmp-1)/2,prod(tmp))
      }
    }
  }
  ## Diagonal part of the rate matrix
  for (i in 1:nSt){
    RateM[i,i] <- -sum(RateM[i,])
  }
  return(list(RateM=RateM,StSpM=StSpM))
}

#' \code{block_counting_process} return a the block counting process for a given sample size
#' as a \code{mult_cont_phase_type} object.
#'
#' @param n Number of samples
#'
#' @return ph_rew_ob
#' A \code{mult_cont_phase_type} representation of the block counting process of size n
#'
#' @examples
#' block_counting_process(8)
#'
#' @export
block_counting_process <- function(n){

  RMASS = RateMAndStateSpace(n)
  m = dim(RMASS$RateM)[1] #(m should be equal to partitions::P(n))
  # Obtain subintensity matrix
  subintensity_matrix = RMASS$RateM[1:(m-1),1:(m-1)]

  # The reward matrix is the state space matrix of the block counting process, except the row & column related to the
  # absorbing state.
  rew_mat = RMASS$StSpM[1:(m-1),1:(n-1)]

  ph_obj=PH(subintensity_matrix)
  ph_rew_obj=MPH(subintensity_matrix, reward_mat = rew_mat)
  return(ph_rew_obj)
}
