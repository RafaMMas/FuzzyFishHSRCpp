% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Zero.order.TSK.plots.R
\name{computeMFValue}
\alias{computeMFValue}
\title{Compute Membership Function (MF) Values (Internal Helper)}
\usage{
computeMFValue(x, type, params)
}
\arguments{
\item{x}{Numeric. Input value or vector of values at which to compute the membership.}

\item{type}{Character. The type of membership function to use (e.g., "trimf", "trapmf", etc.).}

\item{params}{Numeric vector. Parameters for the specified membership function. See details above.}
}
\value{
A numeric vector of membership values corresponding to input \code{x}.
}
\description{
Internal helper function to compute the membership value(s) for a given input
using a specified membership function (MF) type and parameters. Not intended
for use outside of other functions within the package.
}
\details{
Supported MF types:
\itemize{
\item \code{"trimf"}: Triangular MF. \code{params = c(a, b, c)}
\item \code{"trapmf"}: Trapezoidal MF. \code{params = c(a, b, c, d)}
\item \code{"gaussmf"}: Gaussian MF. \code{params = c(mean, sigma)}
\item \code{"sigmf"}: Sigmoidal MF. \code{params = c(a, c)} where \code{a} is slope, \code{c} is center
\item \code{"gbellmf"}: Generalized bell MF. \code{params = c(a, b, c)}
\item \code{"zmf"}: Z-shaped MF. \code{params = c(a, b)}
\item \code{"smf"}: S-shaped MF. \code{params = c(a, b)}
\item \code{"pimf"}: PI-shaped MF. \code{params = c(a, b, c, d)}
}
}
\examples{
computeMFValue(0.5, "trimf", c(0, 0.5, 1))
computeMFValue(seq(0, 1, 0.1), "gaussmf", c(0.5, 0.1))

}
\keyword{internal}
