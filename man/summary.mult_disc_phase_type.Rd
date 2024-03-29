% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary.mult_disc_phase_type}
\alias{summary.mult_disc_phase_type}
\title{Pretty summary of the \code{mult_dist_phase_type} class.}
\usage{
\method{summary}{mult_disc_phase_type}(object, ...)
}
\arguments{
\item{object}{a mult_dist_phase_type object}

\item{...}{other arguments passed to methods}
}
\value{
This function prints a nicely-formatted summary of
a \code{mult_dist_phase_type} object. The summary includes the
sub-intensity matrix, the initial probabilities, the defect,
the reward matrix,
the mean and the (co)variance of the phase-type object.
}
\description{
Pretty summary of the \code{mult_dist_phase_type} class.
}
\examples{

subint <- matrix(c(0.4, 0, 0.2,
                   0.5, 0.3, 0.2,
                   0, 0.7, 0.2), ncol = 3)
R <- matrix(c(0, 1, 1,
              2, 1, 5,
              0, 1, 10,
              1, 2, 3), nrow = 3)
mdph <- MDPH(subint, reward_mat = R)

summary(mdph)

}
