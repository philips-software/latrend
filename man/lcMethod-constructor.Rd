% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method.R
\name{lcMethod-constructor}
\alias{lcMethod-constructor}
\alias{lcMethod}
\title{Create a lcMethod object of the specified type and arguments}
\usage{
lcMethod(.class, ..., .defaults = list(), .excludeArgs = c())
}
\arguments{
\item{.class}{The type of \code{lcMethod} class}

\item{...}{Any arguments to assign to the method object.}

\item{.defaults}{See \code{defaults} of \link{lcMethod.call}.}

\item{.excludeArgs}{See \code{excludeArgs} of \link{lcMethod.call}.}
}
\description{
Note, see the \linkS4class{lcMethod} help page for documentation about the \code{lcMethod} class.

Provides a mechanism for creating \code{lcMethod} objects for an arbitrary class.
However, it is advisable to use the subclass-specific constructors instead (e.g., \code{\link[=lcMethodLcmmGBTM]{lcMethodLcmmGBTM()}}, \code{\link[=lcMethodKML]{lcMethodKML()}}, or \code{\link[=lcMethodLMKM]{lcMethodLMKM()}}).
}
\section{Implementation}{

When creating your own \code{lcMethod} subclass, it is recommended to provide a constructor function for the subclass.
Sensible default method argument values are are specified through the function arguments.

The \code{\link[=lcMethod.call]{lcMethod.call()}} function provides a convenient way to initialize the method according to the user-specified and default function arguments.

\preformatted{
lcMethodExample <- function(
  response,
  time = getOption("latrend.time"),
  id = getOption("latrend.id"),
  nClusters = 2,
  # custom method argument defaults
  extraNum = 1,
  myOptAlgo = "EM",
  algoFunction = mean,
  ...
) {
  lcMethod.call(
    "lcMethodExample",
    call = stackoverflow::match.call.defaults()
  )
}
}

Users can then specify \code{lcMethodExample} objects using the \code{lcMethodExample()} function.
For example, by calling \code{lcMethodExample(response = "Y", nClusters = 3)}.
}

\seealso{
\linkS4class{lcMethod} \link{lcMethod.call}
}
