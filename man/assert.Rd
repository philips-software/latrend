% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assert.R
\name{latrend-assert}
\alias{latrend-assert}
\alias{has_lcMethod_args}
\alias{is_valid_postprob}
\title{latrend-specific assertions}
\usage{
has_lcMethod_args(object, which)

is_valid_postprob(pp, model = NULL)
}
\arguments{
\item{object}{The object to test.}

\item{which}{The argument names. Ellipsis (\code{...}) will be ignored.}

\item{pp}{The posterior probability \code{matrix}.}

\item{model}{The \code{lcModel} object. Optional.}
}
\description{
Assertions and checks that may be of use for custom model implementations.

Check whether the input is a valid posterior probability matrix (for the given model).
}
\keyword{internal}
