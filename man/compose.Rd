% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method.R
\name{compose}
\alias{compose}
\alias{compose,lcMethod-method}
\title{Compose an lcMethod object}
\usage{
\S4method{compose}{lcMethod}(method, envir = NULL)
}
\arguments{
\item{method}{The \code{lcMethod} object.}

\item{envir}{The \code{environment} in which the \code{lcMethod} should be evaluated}
}
\value{
The evaluated and finalized \code{lcMethod} object.
}
\description{
Evaluate and finalize the arguments of the given object inheriting from \code{lcMethod}.
The default implementation returns an updated object with all arguments having been evaluated.
}
\section{Implementation}{

In general, there is no need to extend this method for a specific method, as all arguments are automatically evaluated by the \verb{compose,lcMethod} method.

However, in case there is a need to extend processing or to prevent evaluation of specific arguments (e.g., for handling errors), the method can be overridden for the specific \code{lcMethod} subclass.
\preformatted{
setMethod("compose", "lcMethodExample", function(method, envir = NULL) {
  newMethod <- callNextMethod()
  # further processing
  return (newMethod)
})
}
}

\section{Fitting procedure}{

Each \code{lcMethod} subclass defines a type of methods in terms of a series of steps for estimating the method.
These steps, as part of the fitting procedure, are executed by \code{\link[=latrend]{latrend()}} in the following order:
\enumerate{
\item \code{\link[=compose]{compose()}}: Evaluate and finalize the method argument values.
\item \code{\link[=validate]{validate()}}: Check the validity of the method argument values.
\item \code{\link[=prepareData]{prepareData()}}: Process the training data for fitting.
\item \code{\link[=preFit]{preFit()}}: Prepare environment for estimation, independent of training data.
\item \code{\link[=fit]{fit()}}: Estimate the specified method on the training data, outputting an object inheriting from \code{lcModel}.
\item \code{\link[=postFit]{postFit()}}: Post-process the outputted \code{lcModel} object.
}

The result of the fitting procedure is an \linkS4class{lcModel} object that inherits from the \code{lcModel} class.
}

\seealso{
\link{evaluate.lcMethod}
}
