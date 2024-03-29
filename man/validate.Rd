% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/method.R
\name{validate}
\alias{validate}
\alias{validate,lcMethod-method}
\title{\code{lcMethod} estimation step: method argument validation logic}
\usage{
validate(method, data, envir, ...)

\S4method{validate}{lcMethod}(method, data, envir = NULL, ...)
}
\arguments{
\item{method}{An object inheriting from \code{lcMethod} with all its arguments having been evaluated and finalized.}

\item{data}{A \code{data.frame} representing the transformed training data.}

\item{envir}{The \code{environment} in which the \code{lcMethod} should be evaluated}

\item{...}{Not used.}
}
\value{
Either \code{TRUE} if all validation checks passed,
or a \verb{scalar character} containing a description of the failed validation checks.
}
\description{
Note: this function should not be called directly, as it is part of the \code{lcMethod} \link[=lcMethod-estimation]{estimation procedure}.
For fitting an \code{lcMethod} object to a dataset, use the \code{\link[=latrend]{latrend()}} function or \link[=latrend-estimation]{one of the other standard estimation functions}.

The \code{validate()} function of the \code{lcMethod} object validates the method with respect to the training data.
This enables a method to verify, for example:
\itemize{
\item whether the formula covariates are present.
\item whether the argument combination settings are valid.
\item whether the data is suitable for training.
}

By default, the \code{validate()} function checks whether the id, time, and response variables are present as columns in the training data.
}
\section{Implementation}{

An example implementation checking for the existence of specific arguments and type:
\preformatted{

library(assertthat)
setMethod("validate", "lcMethodExample", function(method, data, envir = NULL, ...) {
  validate_that(
    hasName(method, "myArgument"),
    hasName(method, "anotherArgument"),
    is.numeric(method$myArgument)
  )
})
}
}

\section{Estimation procedure}{

The steps for estimating a \code{lcMethod} object are defined and executed as follows:
\enumerate{
\item \code{\link[=compose]{compose()}}: Evaluate and finalize the method argument values.
\item \code{\link[=validate]{validate()}}: Check the validity of the method argument values in relation to the dataset.
\item \code{\link[=prepareData]{prepareData()}}: Process the training data for fitting.
\item \code{\link[=preFit]{preFit()}}: Prepare environment for estimation, independent of training data.
\item \code{\link[=fit]{fit()}}: Estimate the specified method on the training data, outputting an object inheriting from \code{lcModel}.
\item \code{\link[=postFit]{postFit()}}: Post-process the outputted \code{lcModel} object.
}

The result of the fitting procedure is an \linkS4class{lcModel} object that inherits from the \code{lcModel} class.
}

\seealso{
\link[assertthat:validate_that]{assertthat::validate_that}
}
