% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary.disc_phase_type}
\alias{summary.disc_phase_type}
\title{Pretty summary of the \code{disc_phase_type} class.}
\usage{
\method{summary}{disc_phase_type}(object, ...)
}
\arguments{
\item{object}{a disc_phase_type object}

\item{...}{other arguments passed to methods}
}
\value{
This function prints a nicely-formatted summary of
a \code{disc_phase_type} object. The summary includes the
sub-intensity matrix, the initial probabilities, the defect,
the mean and the variance of the phase-type object.
}
\description{
Pretty summary of the \code{disc_phase_type} class.
}
\examples{

dph <- DPH(matrix(c(0.4, 0, 0.2,
                    0.5, 0.3, 0.2,
                    0, 0.7, 0.2), ncol = 3))

summary(dph)

}
