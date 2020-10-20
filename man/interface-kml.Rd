% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methodKML.R, R/modelKML.R
\docType{class}
\name{interface-kml}
\alias{interface-kml}
\alias{getName,lcMethodKML-method}
\alias{getShortName,lcMethodKML-method}
\alias{preFit,lcMethodKML-method}
\alias{fit,lcMethodKML-method}
\alias{lcModelKML-class}
\alias{clusterTrajectories,lcModelKML-method}
\alias{converged,lcModelKML-method}
\alias{logLik.lcModelKML}
\alias{postprob,lcModelKML-method}
\alias{predictPostprob,lcModelKML-method}
\title{kml interface}
\usage{
\S4method{getName}{lcMethodKML}(object)

\S4method{getShortName}{lcMethodKML}(object)

\S4method{preFit}{lcMethodKML}(method, data, envir, verbose, ...)

\S4method{fit}{lcMethodKML}(method, data, envir, verbose, ...)

\S4method{clusterTrajectories}{lcModelKML}(object, at = time(object), ...)

\S4method{converged}{lcModelKML}(object)

\method{logLik}{lcModelKML}(object, ...)

\S4method{postprob}{lcModelKML}(object)

\S4method{predictPostprob}{lcModelKML}(object, newdata = NULL, ...)
}
\arguments{
\item{object}{The \code{lcMethod} object.}

\item{method}{The \code{lcMethod} object.}

\item{data}{The data, as a \code{data.frame}, on which the model will be trained.}

\item{envir}{The \code{environment} in which the \code{lcMethod} should be evaluated}

\item{verbose}{A \link[R.utils:Verbose]{R.utils::Verbose} object indicating the level of verbosity.}

\item{...}{Additional arguments.}

\item{at}{An optional vector, list or data frame of covariates at which to compute the cluster trajectory predictions.
If a vector is specified, this is assumed to be the time covariate. Otherwise, a named list or data frame must be provided.}

\item{newdata}{Optional data frame for which to compute the posterior probability. If omitted, the model training data is used.}
}
\description{
kml interface
}
\seealso{
\link{lcMethodKML} \link[kml]{kml}
}