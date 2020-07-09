#'Numerical Approximation of charaterstic function
#'
#'\code{ApproxCDF} approximates the cdf F when given a characteristic function phi of a centered random variable, using the formula found in Waller (1995) with
#'original reference to Bohman (1974). The procedure can be numerically unstable in the tails of the distribution, so
#'only the center of the approximation is returned. Some simplifying approximations explained in "Numerical inversion of laplace transform and characteristic function"
#'are used. Note that phi should have a finite first moment.
#'
#'@param phi the characteristic function to be inverted
#'@param H A total of 2H+1 values of F are approximated. By default H of these values are returned unless an interval is provided.
#'@param eta A scaling paramter. By default equidistant points in the interval (-2*pi/eta,2*pi/(eta)) are approximated.
#
#'@param xlim (optional) limits on the x-axis
#'
#'@examples
#'phi <- function(t) exp(-t^2/2)
#'appvals=ApproxCDF(phi,H=1000,eta=0.5,xlim=c(-3,3))
#'plot(appvals[[1]],appvals[[2]],type="l",lwd=2)
#'lines(appvals[[1]],pnorm(appvals[[1]]),type="l",col="red")
#'
#'phi <- function(t) sqrt(2)*abs(t)*besselK(sqrt(2)*abs(t),1)
#'appvals=ApproxCDF(phi,H=10000,eta=0.1,xlim=c(-3,3))
#'plot(appvals[[1]],appvals[[2]],type="l",lwd=2)
#'lines(appvals[[1]],pt(appvals[[1]],df=2),type="l",col="red")
#'
#'@export
ApproxCDF = function(phi,H=2000,eta=0.5,xlim=NULL) {
  z_vals = rep(0,H)
  co = 1
  for(n in 1:(H-1))  {
    z_vals[co+1]= phi(eta*n)/(pi*1i*(n)) #start at index 2 - the first value of z is 0
    co = co + 1
  }

  yvals_pos=0.5+(1:H)/H-Re(fft(z_vals,inverse=FALSE))
  yvals_neg=0.5-(1:H)/H-Re(fft(z_vals,inverse=TRUE))
  xvals_pos = 2*pi*(1:H)/(eta*H)
  xvals_neg = -xvals_pos
  xvals_neg = rev(xvals_neg)
  yvals_neg = rev(yvals_neg)
  xvals = c(xvals_neg,xvals_pos)
  yvals = c(yvals_neg,yvals_pos)

  if(!is.null(xlim)) {
    indexes = intersect(which(xvals>xlim[1]),which(xvals<xlim[2]))
  }
  else {
    indexes = (1:(H+1))+floor( (H-1)/2)
  }

  xvals = xvals[indexes]
  yvals = yvals[indexes]
  return(list(xvals,yvals))
}
