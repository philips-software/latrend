% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{plotFittedTrajectories}
\alias{plotFittedTrajectories}
\alias{plotFittedTrajectories,lcModel-method}
\title{Plot fitted trajectories of a lcModel}
\usage{
\S4method{plotFittedTrajectories}{lcModel}(object, ...)
}
\arguments{
\item{object}{The \code{lcModel} object.}

\item{...}{
  Arguments passed on to \code{\link[=trajectories]{trajectories}}
  \describe{
    \item{\code{}}{}
  }}
}
\description{
Plot fitted trajectories of a lcModel
}
\examples{
data(latrendData)
model <- latrend(method = lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
plotFittedTrajectories(model)
}
\seealso{
\link{fittedTrajectories} \link{plotClusterTrajectories} \link{plotTrajectories} \link{plot}
}