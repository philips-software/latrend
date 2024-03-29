% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{fitted.lcModel}
\alias{fitted.lcModel}
\title{Extract lcModel fitted values}
\usage{
\method{fitted}{lcModel}(object, ..., clusters = trajectoryAssignments(object))
}
\arguments{
\item{object}{The \code{lcModel} object.}

\item{...}{Additional arguments.}

\item{clusters}{Optional cluster assignments per id. If unspecified, a \code{matrix} is returned containing the cluster-specific predictions per column.}
}
\value{
A \code{numeric} vector of the fitted values for the respective class, or a \code{matrix} of fitted values for each cluster.
}
\description{
Returns the cluster-specific fitted values for the given \code{lcModel} object.
The default implementation calls \code{\link[=predict]{predict()}} with \code{newdata = NULL}.
}
\section{Implementation}{

Classes extending \code{lcModel} can override this method to adapt the computation of the predicted values for the training data.
Note that the implementation of this function is only needed when \code{\link[=predict]{predict()}} and \code{\link[=predictForCluster]{predictForCluster()}} are not defined for the \code{lcModel} subclass.
\preformatted{
fitted.lcModelExt <- function(object, ..., clusters = trajectoryAssignments(object)) {
  pred = predict(object, newdata = NULL)
  transformFitted(pred = pred, model = object, clusters = clusters)
}
}
The \code{\link[=transformFitted]{transformFitted()}} function takes care of transforming the prediction input to the right output format.
}

\examples{
data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData)
fitted(model)
}
\seealso{
\link{fittedTrajectories} \link{plotFittedTrajectories} \link[stats:fitted.values]{stats::fitted} \link{predict.lcModel} \link{trajectoryAssignments} \link{transformFitted}

Other lcModel functions: 
\code{\link{clusterNames}()},
\code{\link{clusterProportions}()},
\code{\link{clusterSizes}()},
\code{\link{clusterTrajectories}()},
\code{\link{coef.lcModel}()},
\code{\link{converged}()},
\code{\link{deviance.lcModel}()},
\code{\link{df.residual.lcModel}()},
\code{\link{estimationTime}()},
\code{\link{externalMetric}()},
\code{\link{fittedTrajectories}()},
\code{\link{getCall.lcModel}()},
\code{\link{getLcMethod}()},
\code{\link{ids}()},
\code{\link{lcModel-class}},
\code{\link{metric}()},
\code{\link{model.frame.lcModel}()},
\code{\link{nClusters}()},
\code{\link{nIds}()},
\code{\link{nobs.lcModel}()},
\code{\link{plot-lcModel-method}},
\code{\link{plotClusterTrajectories}()},
\code{\link{plotFittedTrajectories}()},
\code{\link{postprob}()},
\code{\link{predict.lcModel}()},
\code{\link{predictAssignments}()},
\code{\link{predictForCluster}()},
\code{\link{predictPostprob}()},
\code{\link{qqPlot}()},
\code{\link{residuals.lcModel}()},
\code{\link{sigma.lcModel}()},
\code{\link{strip}()},
\code{\link{time.lcModel}()},
\code{\link{trajectoryAssignments}()}
}
\concept{lcModel functions}
