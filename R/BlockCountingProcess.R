##-------------------------------------------------------------
## Dependencies: library("partitions")
##-------------------------------------------------------------
##
## Purpose:
## This function finds the state space and corresponding rate matrix
## for the block counting process for a number of samples n in the 
## standard coalescent.
##
## Name: BlockCountingRateMatAndStateSpace
## Author: Andersen and Hobolth
## Date: Spring 2020
##
## Input:
## n: Number of samples
##
## Output:
## List consisting of
## RateM: Rate matrix
## StSpM: Matrix with rows corresponding to the states
##        A state is a n-dimensional row vector (a1,...,an)
##        similar to Ewens sampling formula.
##        We always have a1+2*a2+...+n*an=n
##        The beginning state is (n,0,...,0),
##        the next state is (n-2,1,0,...,0) 
##        with rate (n choose 2), 
##        and the ending state (MRCA) is (0,...,0,1).
##
##----------------------------------------------------------------
BlockCountingRateMAndStateSpace <- function(n){
  ##----------------------------------------------------
  ## Possible states
  ##----------------------------------------------------
  ## Size of the state space (number of states)
  nSt <- P(n)
  ## Definition of the state space
  StSpM <- matrix(ncol=n,nrow=nSt)
  ## Set of partitions of [n]
  x <- parts(n)
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
  ## Algorithm for finding rates between states
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