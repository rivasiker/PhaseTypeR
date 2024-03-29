% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_phase_type.R
\name{check_phase_type}
\alias{check_phase_type}
\title{Checking the basic phase-type assumptions}
\usage{
check_phase_type(subint_mat, init_probs)
}
\arguments{
\item{subint_mat}{Sub-intensity rate matrix.}

\item{init_probs}{Initial probability vector.}
}
\value{
A list with the elements of a phase-type distribution.
}
\description{
This function checks whether all the elements for
building a phase-type object have the right shape.
}
\keyword{internal}
