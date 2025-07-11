% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hill.climbing.extended.R
\name{Hill.climbing.extended}
\alias{Hill.climbing.extended}
\title{Hill-Climbing Optimization with Restarts}
\usage{
Hill.climbing.extended(
  func,
  values = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  size = 8,
  mode = "max",
  restarts = 1,
  seed = NULL
)
}
\arguments{
\item{func}{A function that takes a numeric vector as input and returns a scalar value.
This is the objective function to be optimized.}

\item{values}{A numeric vector specifying the allowed values in the solution sequence.
Defaults to \code{c(0, 0.2, 0.4, 0.6, 0.8, 1)}.}

\item{size}{An integer specifying the length of the solution sequence.}

\item{mode}{A character string indicating whether to \code{"max"}imize or \code{"min"}imize
the objective function. Default is \code{"max"}.}

\item{restarts}{An integer specifying how many independent random restarts to perform.
Higher values increase the chance of escaping local optima. Default is 1.}

\item{seed}{Optional integer seed for reproducibility of random number generation.}
}
\value{
A list with two elements:
\describe{
\item{\code{optimal_sequence}}{The best sequence of values found across all restarts.}
\item{\code{optimal_value}}{The corresponding value of the objective function.}
}
}
\description{
Performs an extended version of the hill-climbing algorithm with support
for multiple random restarts. This optimization method iteratively explores
neighboring sequences in search of a maximum or minimum of a user-defined
objective function.
}
\examples{
# Define a simple objective function
f <- function(x) -sum((x - 0.5)^2)

# Run hill-climbing to maximize f
result <- Hill.climbing.extended(f, size = 6, restarts = 5, seed = 42)
print(result)

}
