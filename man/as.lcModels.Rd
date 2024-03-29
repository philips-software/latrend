% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{as.lcModels}
\alias{as.lcModels}
\title{Convert a list of lcModels to a lcModels list}
\usage{
as.lcModels(x)
}
\arguments{
\item{x}{A \code{list} of \code{lcModel} objects, an \code{lcModels} object, or \code{NULL}.}
}
\value{
A \code{lcModels} object.
}
\description{
Convert a list of lcModels to a lcModels list
}
\section{Functionality}{

\itemize{
\item \link[=print.lcModels]{Print} an argument summary for each of the models.
\item \link[=as.data.frame.lcModels]{Convert} to a \code{data.frame} of method arguments.
\item \link[=subset.lcModels]{Subset} the list.
\item Compute an \link[=metric]{internal metric} or \link[=externalMetric]{external metric}.
\item Obtain the best model according to \link[=min.lcModels]{minimizing} or \link[=max.lcModels]{maximizing} a \link[=latrend-metrics]{metric}.
\item Obtain the summed \link[=estimationTime]{estimation time}.
\item \link[=plotMetric]{Plot a metric} across a variable.
\item \link[=plotClusterTrajectories]{Plot the cluster trajectories}.
}
}

\seealso{
lcModels

Other lcModels functions: 
\code{\link{lcModels}},
\code{\link{lcModels-class}},
\code{\link{max.lcModels}()},
\code{\link{min.lcModels}()},
\code{\link{plotMetric}()},
\code{\link{print.lcModels}()},
\code{\link{subset.lcModels}()}
}
\concept{lcModels functions}
