% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{clusterTrajectories}
\alias{clusterTrajectories}
\alias{clusterTrajectories,lcModel-method}
\title{Extract the cluster trajectories}
\usage{
\S4method{clusterTrajectories}{lcModel}(object, at = time(object), what = "mu", ...)
}
\arguments{
\item{object}{The \code{lcModel} object.}

\item{at}{An optional vector, list or data frame of covariates at which to compute the cluster trajectory predictions.
If a vector is specified, this is assumed to be the time covariate. Otherwise, a named list or data frame must be provided.}

\item{what}{The distributional parameter to predict. By default, the mean response 'mu' is predicted. The cluster membership predictions can be obtained by specifying \code{what = 'mb'}.}

\item{...}{Additional arguments.}
}
\value{
A data.frame of the estimated values at the given times. The first column should be named "Cluster". The second column should be time, with the name matching the \code{timeVariable(object)}. The third column should be the expected value of the observations, named after the \code{responseVariable(object)}.
}
\description{
Extracts a data frame of all cluster trajectories.
}
\examples{
model <- latrend(method = lcMethodLcmmGMM(fixed = Y ~ Time, mixture = fixed),
  id = "Id", time = "Time", data = latrendData)
clusterTrajectories(model)

clusterTrajectories(model, at = c(0, .5, 1))
}
\seealso{
Other model-specific methods: 
\code{\link{coef.lcModel}()},
\code{\link{converged}()},
\code{\link{deviance.lcModel}()},
\code{\link{df.residual.lcModel}()},
\code{\link{fitted.lcModel}()},
\code{\link{fittedTrajectories}()},
\code{\link{lcModel-class}},
\code{\link{logLik.lcModel}()},
\code{\link{model.frame.lcModel}()},
\code{\link{nobs.lcModel}()},
\code{\link{postprob}()},
\code{\link{predict.lcModel}()},
\code{\link{predictAssignments}()},
\code{\link{predictForCluster}()},
\code{\link{predictPostprob}()},
\code{\link{residuals.lcModel}()},
\code{\link{sigma.lcModel}()},
\code{\link{time.lcModel}()}
}
\concept{model-specific methods}
