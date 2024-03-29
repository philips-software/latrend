% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/trajectories.R, R/model.R
\name{plotTrajectories}
\alias{plotTrajectories}
\alias{plotTrajectories,data.frame-method}
\alias{plotTrajectories,ANY-method}
\alias{plotTrajectories,lcModel-method}
\title{Plot the data trajectories}
\usage{
plotTrajectories(object, ...)

\S4method{plotTrajectories}{data.frame}(
  object,
  response,
  time = getOption("latrend.time"),
  id = getOption("latrend.id"),
  cluster = NULL,
  facet = TRUE,
  ...
)

\S4method{plotTrajectories}{ANY}(object, ...)

\S4method{plotTrajectories}{lcModel}(object, ...)
}
\arguments{
\item{object}{The data or model or extract the trajectories from.}

\item{...}{
  Arguments passed on to \code{\link[=trajectories]{trajectories}}
  \describe{
    \item{\code{}}{}
  }}

\item{response}{Response variable \code{character} name or a \code{call}.}

\item{time}{The time variable name, see \link{timeVariable}.}

\item{id}{The identifier variable name, see \link{idVariable}.}

\item{cluster}{Cluster variable name. If unspecified, trajectories are not grouped.
Alternatively, cluster is a vector indicating cluster membership per id.}

\item{facet}{Whether to facet by cluster.}
}
\description{
Plots the output of \link{trajectories} for the given object.
}
\examples{
data(latrendData)

if (require("ggplot2")) {
  plotTrajectories(latrendData, response = "Y", id = "Id", time = "Time")

  plotTrajectories(
    latrendData,
    response = quote(exp(Y)),
    id = "Id",
    time = "Time"
  )

  plotTrajectories(
    latrendData,
    response = "Y",
    id = "Id",
    time = "Time",
    cluster = "Class"
  )

  # compute cluster membership based on the mean being below 0
  assignments <- aggregate(Y ~ Id, latrendData, mean)$Y < 0
  plotTrajectories(
    latrendData,
    response = "Y",
    id = "Id",
    time = "Time",
    cluster = assignments
  )
}
data(latrendData)
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
model <- latrend(method, latrendData, nClusters = 3)

if (require("ggplot2")) {
  plotTrajectories(model)
}
}
\seealso{
\link{trajectories}

\link{trajectories} \link{plotFittedTrajectories} \link{plotClusterTrajectories}

\link{trajectories}
}
