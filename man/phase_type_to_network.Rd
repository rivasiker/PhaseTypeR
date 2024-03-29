% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phase_type_to_network.R
\name{phase_type_to_network}
\alias{phase_type_to_network}
\title{Phase-type distribution to network}
\usage{
phase_type_to_network(phase_type, t = NULL)
}
\arguments{
\item{phase_type}{an object of class \code{disc_phase_type} or
\code{cont_phase_type}}

\item{t}{NULL or numeric. Sampling time for the continuous phase-type distribution.}
}
\value{
An igraph graph object of the phase-type distribution.
}
\description{
This function converts a phase-type distribution into an
igraph graph object.
}
\examples{
\dontrun{
cont_phase_type <- matrix(c(-3, 0, 1,
                            2, -3, 1,
                            1, 1, -2), ncol = 3)
Y <- PH(cont_phase_type)
Y_network <- phase_type_to_network(Y)
set.seed(28)
plot(Y_network, layout = layout_with_fr(Y_network,  weights = rep(1, length(E(Y_network)))))
}

}
