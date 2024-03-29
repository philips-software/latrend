% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{lcMethods}
\alias{lcMethods}
\title{Generate a list of lcMethod objects}
\usage{
lcMethods(method, ..., envir = NULL)
}
\arguments{
\item{method}{The \code{lcMethod} to use as the template, which will be updated for each of the other arguments.}

\item{...}{Any other arguments to update the \code{lcMethod} definition with. Values must be \code{scalar}, \code{vector}, \code{list}, or encapsulated in a \code{.()} call.
Arguments wrapped in \code{.()} are passed as-is to the model call, ensuring a readable method.
Arguments comprising a single \code{symbol} (e.g. a variable name) are interpreted as a constant. To force evaluation, specify \code{arg=(var)} or \code{arg=force(var)}.
Arguments of type \code{vector} or \code{list} are split across a series of method fit calls.
Arguments of type \code{scalar} are constant across the method fits.
If a \code{list} is intended to be passed as a constant argument, then specifying \code{arg=.(listObject)} results in it being treated as such.}

\item{envir}{The \code{environment} in which to evaluate the method arguments.}
}
\value{
A \code{list} of \code{lcMethod} objects.
}
\description{
Generates a list of \code{lcMethod} objects for all combinations of the provided argument values.
}
\examples{
data(latrendData)
baseMethod <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
methods <- lcMethods(baseMethod, nClusters = 1:6)

nclus <- 1:6
methods <- lcMethods(baseMethod, nClusters = nclus)

# list notation, useful for providing functions
methods <- lcMethods(baseMethod, nClusters = .(1, 3, 5))
length(methods) # 3
}
