% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-evaluation.R
\name{OCC}
\alias{OCC}
\title{Odds of correct classification (OCC)}
\usage{
OCC(object)
}
\arguments{
\item{object}{The model, of type \code{lcModel}.}
}
\value{
The OCC per cluster, as a \verb{numeric vector} of length \code{nClusters(object)}.
Empty clusters will output \code{NA}.
}
\description{
Computes the odds of correct classification (OCC) for each cluster.
In other words, it computes the proportion of trajectories that can be expected to be correctly classified by the model for each cluster.
}
\details{
An OCC of 1 indicates that the cluster assignment is no better than by random chance.
}
\references{
\insertRef{nagin2005group}{latrend}
\insertRef{klijn2017introducing}{latrend}
\insertRef{vandernest2020overview}{latrend}
}
\seealso{
\link{confusionMatrix} \link{APPA}
}
