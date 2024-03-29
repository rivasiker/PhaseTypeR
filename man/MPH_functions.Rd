% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MPH_functions.R
\name{MPH_functions}
\alias{MPH_functions}
\alias{dMPH}
\alias{qMPH}
\alias{pMPH}
\alias{rMPH}
\alias{rFullMPH}
\title{The Multivariate Continuous Phase-Type Distribution}
\usage{
dMPH(x, obj)

qMPH(p, obj)

pMPH(q, obj)

rMPH(n, obj)

rFullMPH(obj)
}
\arguments{
\item{x, q}{vector of quantiles.}

\item{obj}{an object of class \code{mult_cont_phase_type}.}

\item{p}{vector of probabilities.}

\item{n}{number of observations. If length(n) > 1, the length is taken to be
the number required.}
}
\value{
\code{dMPH} gives the density, \code{pMPH} gives the
distribution function, \code{qMPH} gives the quantile function,
and \code{rMPH} generates random deviates. \code{rFullMPH} returns
the full path of a random draw from the distribution.

Each row of the result of For \code{dMPH}, \code{pMPH}, \code{qMPH}, and
\code{rMPH} corresponds to each univariate reward transformation.
For \code{dMDPH}, \code{qMDPH} and \code{pMDPH}, the inputs \code{x},
\code{p} and \code{q} can be matrices where in row i the i_th reward
transformation and in col j the j_th value of \code{x}, \code{p} or \code{q}
tested.

The length of the result is determined by \code{n} for \code{rMPH},
and is the maximum of the lengths of the numerical arguments for the other
functions.

The numerical arguments other than \code{n} are recycled to the length of the
result. Only the first elements of the logical arguments are used.
}
\description{
Density, distribution function, quantile function and random generation
for the multivariate continuous phase-type distribution.
}
\section{Functions}{
\itemize{
\item \code{dMPH()}: Density function for the multivariate continuous phase-type distribution.

\item \code{qMPH()}: Quantile function for the multivariate continuous phase-type distribution.

\item \code{pMPH()}: Distribution function for the multivariate continuous phase-type distribution.

\item \code{rMPH()}: Random number generator for the multivariate continuous phase-type distribution.

\item \code{rFullMPH()}: Simulation of the full path for the multivariate continuous phase-type distribution.

}}
\examples{

cont_phase_type <- matrix(c(-3, 0, 1,
                            2, -3, 1,
                            1, 1, -2), ncol = 3)
R <- matrix(c(0, 1, 1,  2,
                 2, 1, 5,  2,
                 0, 1, 10, 2), nrow = 3, ncol=4, byrow=TRUE)
Y <- MPH(cont_phase_type, reward_mat = R)

dMPH(3:4, Y)
pMPH(1.45, Y)
qMPH(0.5, Y)
set.seed(0)
rMPH(6, Y)
rFullMPH(Y)

}
\seealso{
\link[stats]{Distributions} for other standard distributions.
}
