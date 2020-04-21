##---------------------------------------------------------
## Discrete phase-type distributions and segregating sites
##---------------------------------------------------------
#setwd("C:/Users/asger/Documents/phasty/R")
#library("partitions")
#library("expm")
#source("BlockCountingProcess.R")
#source("MultivariatePhaseType.R")
##------------------------------------------------
## We consider a generalization of the construction of the
## reward-transformed (non-negative and integer valued)
## DPH representation given by Navarro (2018).
##-------------------------------------------------
DPHIntegerWeightedSFS <- function(n,RewV,tht,kMax){
  RtSt <- BlockCountingRateMAndStateSpace(n)
  ## Number of transient states
  nTrSt <- nrow(RtSt$RateM)-1
  RtM <- RtSt$RateM ; SubRtM <- RtM[1:nTrSt,1:nTrSt]
  ## Define DPH-representation of mutations in each state
  ## Multiply by diagonal of number of lineages
  nBrnchs <- rowSums(RtSt$StSpM)[1:nTrSt]
  SubRtM <- diag(1/nBrnchs)%*%SubRtM
  SubTrM <- solve( diag(1,nrow=nTrSt)-SubRtM/(tht/2) )
  # SubTrM %*% rep(1,nTrSt)
  ExitPrbV <- as.vector(1-SubTrM %*% rep(1,nTrSt) )
  ##-----------------------------------------------------
  ## Construct new Markov chain
  maxRwV <- rep(0,nTrSt)
  for (i in 1:nTrSt){
    maxRwV[i] <- max( RewV[ which(RtSt$StSpM[i,]>0) ] )
  }
  nNewTrSt <- sum(maxRwV)
  NewSubTrM <- matrix(0,nrow=nNewTrSt,ncol=nNewTrSt)
  # cat("Dimension of new Markov chain:",nNewTrSt,"\n")
  ## The block structure implies that the
  ## upper diagonal matrix is 1
  for (i in 1:(nrow(NewSubTrM)-1)){
    NewSubTrM[i,i+1] <- 1
  }
  ## The lower and higher indices for the blocks are BLow and BHgh
  ## Probabilities for moving in the block-extended matrix
  BLow <- cumsum(maxRwV[1:nTrSt])
  BHgh <- cumsum(c(1,maxRwV[1:nTrSt]))
  ## The rows where the blocks end are set to zero
  for (i in 1:nTrSt){
    NewSubTrM[BLow[i],] <- 0
  }
  ## The low rows of the blocks are defined
  for (i in 1:nTrSt){
    for (j in i:nTrSt){
      # cat(i,j,"\n")
      wh <- which(RtSt$StSpM[j,]>0)
      if (length(wh)>1){
        tbl <- tabulate(rep(RewV[wh],times=RtSt$StSpM[j,wh]))
        nrm.tbl <- tbl/sum(tbl)
        NewSubTrM[BLow[i],BHgh[j]:BLow[j]] <- SubTrM[i,j]*rev(nrm.tbl)
      }
      if (length(wh)==1){
        NewSubTrM[BLow[i],BHgh[j]] <- SubTrM[i,j]
      }
    }
  }
  NewExitPrbV <- as.vector(1-NewSubTrM %*% rep(1,nNewTrSt) )
  # kMax <- 50
  NewPrb <- rep(0,kMax+1)
  BgnPrb <- rep(0,nNewTrSt) ; BgnPrb[BLow[1]] <- 1
  for (k in 1:(kMax+1)){
    NewPrb[k] <- BgnPrb%*%(NewSubTrM%^%(k-1))%*%NewExitPrbV
  }
  ##-------------------------------
  out <- list()
  out$NewPrb <- NewPrb
  out$NewSubTrM <- NewSubTrM
  out$SubTrM <- SubTrM
  return(out)
}
##---------------------------------------------------------
n=4 ; tht=1
## Pairwise integer weights to singletons, doubletons etc.
RewVP <- (1:(n-1))*((n-1):1)
## L integer weights to singletons, doubletons etc.
RewVL <- 1:(n-1)
## H integer weights to singletons, doubletons etc.
RewVH <- ( 1:(n-1) )^2
PrbP <- DPHIntegerWeightedSFS(n=n,RewV=RewVP,tht=tht,kMax=100)$NewPrb
cat("Done with Pairwise!...","\n")
PrbL <- DPHIntegerWeightedSFS(n=n,RewV=RewVL,tht=tht,kMax=100)$NewPrb
cat("Done with L!...","\n")
PrbH <- DPHIntegerWeightedSFS(n=n,RewV=RewVH,tht=tht,kMax=100)$NewPrb
cat("Done with H!...","\n")
##---------------------------------------------
## Plotting
##----------------------------------------------
#file.name <- paste("IntegerDist",n,".pdf",sep="")
#pdf(file.name,width=9,height=7)
kMx <- 50
main.txt <- paste("Integer-weighted SFS distributions for n=",n,"and theta=",tht)
plot(0:kMx/(n*(n-1)/2),PrbP[1:(kMx+1)],type="l",lty="dotted",xlim=c(0,2),
     xlab="Weighted SFS",ylab="Probability",
     main=main.txt,cex.main=1.2,cex.lab=1.2,lwd=2)
points(0:kMx/(n*(n-1)/2),PrbP[1:(kMx+1)],pch=19,cex=.8)
points(0:kMx/(n*(n-1)/2),PrbH[1:(kMx+1)],type="l",lty="solid",lwd=2)
points(0:kMx/(n*(n-1)/2),PrbH[1:(kMx+1)],pch=19,cex=.8)
points(0:kMx/(n-1),PrbL[1:(kMx+1)],type="l",lty="dashed",lwd=2)
points(0:kMx/(n-1),PrbL[1:(kMx+1)],pch=19,cex=.8)
abline(v=tht,col="gray")
txt <- paste("mean=theta=",tht)
text(tht,PrbP[1],txt,pos=2,cex=1.2)
legend("topright",c("Pairwise","H","L"),lwd=2,
       lty=c("dotted","solid","dashed"),bty="n",cex=1.2)
#dev.off()
##------------------------------
## Check mean and variance
##------------------------------
## Theoretical mean:
cat("Mean P from theory:", tht*n*(n-1)/2,"\n")
cat("Mean L from theory", tht*(n-1),"\n")
cat("Mean H from theory", tht*n*(n-1)/2,"\n")
##---------------------------------------------------------
## Theoretical variance:
BCP <- BlockCountingRateMAndStateSpace(n)
## Reduce to subintensity matrix and corresponding rewards
nSt <- dim(BCP$RateM)[1]
subIntMat <- BCP$RateM[1:(nSt-1),1:(nSt-1)]
rewardMat <- BCP$StSpM[1:(nSt-1),1:(dim(BCP$StSpM)[2]-1)]
cvmat <- CovMatMPH(initVec=c(1,rep(0,nSt-2)),subIntMat=subIntMat,rewardMat=rewardMat)
mnvec <- as.vector( 1/(1:(n-1)) )
fullcvmat <- (tht^2/4)*cvmat+(tht/2)*2*diag(mnvec)
#RewVP <- (1:(n-1))*((n-1):1)
#RewVL <- 1:(n-1)
#RewVH <- ( 1:(n-1) )^2
cat("Variance P from MPH theory:",RewVP %*% fullcvmat %*% RewVP,"\n")
cat("Variance L from MPH theory:",RewVL %*% fullcvmat %*% RewVL,"\n")
cat("Variance L from MPH theory:",RewVH %*% fullcvmat %*% RewVH,"\n")
##------------------------------------------------------------------
## DPH mean and variance from distribution defined on integers (zero included):
MnVrFromIntegerDst <- function(PrbV){
  ln <- length(PrbV)
  mn <- sum(PrbV*(0:(ln-1)))
  mnsqr <- sum(PrbV*(0:(ln-1))^2)
  vr <- mnsqr-mn^2
  return(c(mn,vr))
}
cat("P mean and variance from DPH distribution:",MnVrFromIntegerDst(PrbP),"\n")
cat("L mean and variance from DPH distribution:",MnVrFromIntegerDst(PrbL),"\n")
cat("H mean and variance from DPH distribution:",MnVrFromIntegerDst(PrbH),"\n")


