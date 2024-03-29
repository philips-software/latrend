% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methodFlexmix.R, R/methodFlexmixGBTM.R,
%   R/modelFlexmix.R
\name{interface-flexmix}
\alias{interface-flexmix}
\alias{getArgumentDefaults,lcMethodFlexmix-method}
\alias{getArgumentExclusions,lcMethodFlexmix-method}
\alias{getCitation,lcMethodFlexmix-method}
\alias{getName,lcMethodFlexmix-method}
\alias{getShortName,lcMethodFlexmix-method}
\alias{preFit,lcMethodFlexmix-method}
\alias{fit,lcMethodFlexmix-method}
\alias{getArgumentDefaults,lcMethodFlexmixGBTM-method}
\alias{getArgumentExclusions,lcMethodFlexmixGBTM-method}
\alias{getName,lcMethodFlexmixGBTM-method}
\alias{getShortName,lcMethodFlexmixGBTM-method}
\alias{preFit,lcMethodFlexmixGBTM-method}
\alias{fitted.lcModelFlexmix}
\alias{predictForCluster,lcModelFlexmix-method}
\alias{postprob,lcModelFlexmix-method}
\alias{logLik.lcModelFlexmix}
\alias{coef.lcModelFlexmix}
\alias{converged,lcModelFlexmix-method}
\title{flexmix interface}
\usage{
\S4method{getArgumentDefaults}{lcMethodFlexmix}(object)

\S4method{getArgumentExclusions}{lcMethodFlexmix}(object)

\S4method{getCitation}{lcMethodFlexmix}(object, ...)

\S4method{getName}{lcMethodFlexmix}(object)

\S4method{getShortName}{lcMethodFlexmix}(object)

\S4method{preFit}{lcMethodFlexmix}(method, data, envir, verbose, ...)

\S4method{fit}{lcMethodFlexmix}(method, data, envir, verbose, ...)

\S4method{getArgumentDefaults}{lcMethodFlexmixGBTM}(object)

\S4method{getArgumentExclusions}{lcMethodFlexmixGBTM}(object)

\S4method{getName}{lcMethodFlexmixGBTM}(object)

\S4method{getShortName}{lcMethodFlexmixGBTM}(object)

\S4method{preFit}{lcMethodFlexmixGBTM}(method, data, envir, verbose)

\method{fitted}{lcModelFlexmix}(object, ..., clusters = trajectoryAssignments(object))

\S4method{predictForCluster}{lcModelFlexmix}(object, newdata, cluster, what = "mu", ...)

\S4method{postprob}{lcModelFlexmix}(object, ...)

\method{logLik}{lcModelFlexmix}(object, ...)

\method{coef}{lcModelFlexmix}(object, ...)

\S4method{converged}{lcModelFlexmix}(object, ...)
}
\arguments{
\item{object}{The \code{lcModel} object.}

\item{...}{Not used.}

\item{method}{An object inheriting from \code{lcMethod} with all its arguments having been evaluated and finalized.}

\item{data}{A \code{data.frame} representing the transformed training data.}

\item{envir}{The \code{environment} containing variables generated by \code{\link[=prepareData]{prepareData()}} and \code{\link[=preFit]{preFit()}}.}

\item{verbose}{A \link[R.utils:Verbose]{R.utils::Verbose} object indicating the level of verbosity.}

\item{clusters}{Optional cluster assignments per id. If unspecified, a \code{matrix} is returned containing the cluster-specific predictions per column.}

\item{newdata}{A \code{data.frame} of trajectory data for which to compute trajectory assignments.}

\item{cluster}{The cluster name (as \code{character}) to predict for.}

\item{what}{The distributional parameter to predict. By default, the mean response 'mu' is predicted. The cluster membership predictions can be obtained by specifying \code{what = 'mb'}.}
}
\description{
flexmix interface
}
\seealso{
\link{lcMethodFlexmix} \link[flexmix]{flexmix}
}
\keyword{internal}
