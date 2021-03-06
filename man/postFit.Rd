% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method.R
\name{postFit}
\alias{postFit}
\alias{postFit,lcMethod-method}
\title{Post-process the fitted lcModel object}
\usage{
\S4method{postFit}{lcMethod}(method, data, model, envir, verbose)
}
\arguments{
\item{method}{An object inheriting from \code{lcMethod} with all its arguments having been evaluated and finalized.}

\item{data}{A \code{data.frame} representing the transformed training data.}

\item{model}{The \code{lcModel} object returned by \code{\link[=fit]{fit()}}.}

\item{envir}{The \code{environment} containing variables generated by \code{\link[=prepareData]{prepareData()}} and \code{\link[=preFit]{preFit()}}.}

\item{verbose}{A \link[R.utils:Verbose]{R.utils::Verbose} object indicating the level of verbosity.}
}
\value{
The updated \code{lcModel} object.
}
\description{
Post-process the \code{lcModel} object returned by \code{\link[=fit]{fit()}}. This can be used, for example, to (pre)compute additional metrics.
By default, this method does not do anything and returns the original \code{lcModel} object.

This is the last step in the \code{lcMethod} fitting procedure. The \code{postFit} method may be called again on fitted \code{lcModel} objects, allowing post-processing to be updated for existing models.
}
\section{Implementation}{

The method is intended to be able to be called on previously fitted \code{lcModel} objects as well, allowing for potential bugfixes or additions to previously fitted models.
Therefore, when implementing this method, ensure that you do not discard information from the model which would prevent the method from being run a second time on the object.

In this example, the \code{lcModelExample} class is assumed to be defined with a slot named \code{"centers"}:
\preformatted{
setMethod("postFit", "lcMethodExample", function(method, data, model, envir, verbose) {
  # compute and store the cluster centers
  model@centers <- INTENSIVE_COMPUTATION
  return (model)
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

