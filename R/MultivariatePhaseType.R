## Purpose:
## This function determines the covariance between the variables
## in a MPH-distribution, i.e. an implementation of the formulas
## in the bottom of page 440 in Bladt and Nielsen (2017)
##
## Name: CovMatMPH
## Author: Andersen and Hobolth
## Date: Spring 2020
##
## Input:
## Paramters from a MPH distribution:
## initVec: Initial distribution
## subIntMat: Sub-intensity matrix
## rewardMat: Reward matrix
##
## Output:
## Covariance matrix
##---------------------------------------------------------
## Covariance matrix for MPH(initVec,subIntMat,rewardMat)
##---------------------------------------------------------
CovMatMPH <- function(initVec, subIntMat, rewardMat){
  UMat <- -solve(subIntMat) ## Green matrix
  RMat <- rewardMat         ## Reward matrix
  sz <- dim(RMat)[2]
  out = matrix(0, sz, sz)
  for(i in 1:sz) {
    for(j in 1:sz) {
      out[i, j] =
        initVec %*% UMat %*% diag(RMat[, i]) %*% UMat %*%
        matrix(RMat[, j], length(RMat[, j]), 1) + initVec %*% UMat %*%
        diag(RMat[, j]) %*% UMat %*% matrix(RMat[, i], length(RMat[, i]), 1)
    }
  }
  return(out - t(initVec %*% UMat %*% RMat) %*% (initVec %*% UMat %*% RMat))
}
##-------------------------------------------------------
## Mean vector for MPH(initVec,subIntMat,rewardMat)
##-------------------------------------------------------
MeanVecMPH <- function(initVec, subIntMat, rewardMat){
  UMat <- -solve(subIntMat) ## Green matrix
  RMat <- rewardMat         ## Reward matrix
  return(as.vector(initVec %*% UMat %*% RMat))
}
