% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{clusterProportions}
\alias{clusterProportions}
\alias{clusterProportions,lcModel-method}
\title{Proportional size of each cluster}
\usage{
\S4method{clusterProportions}{lcModel}(object, ...)
}
\arguments{
\item{object}{The \code{lcModel} to obtain the proportions from.}

\item{...}{
  Arguments passed on to \code{\link[=postprob]{postprob}}
  \describe{
    \item{\code{}}{}
  }}
}
\value{
A named \verb{numeric vector} of length \code{nClusters(object)} with the proportional size of each cluster.
}
\description{
Obtain the proportional size per cluster, with sizes between 0 and 1.
By default, the cluster proportions are determined from the cluster-averaged posterior probabilities of the fitted data (as computed by the \code{\link[=postprob]{postprob()}} function).
}
\section{Implementation}{

Classes extending \code{lcModel} can override this method to return, for example, the exact estimated mixture proportions based on the model coefficients.
\preformatted{
setMethod("clusterProportions", "lcModelExt", function(object, ...) {
  # return cluster proportion vector
})
}
}

\examples{
data(latrendData)
model <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
clusterProportions(model)
}
\seealso{
\link{clusterSizes} \link{postprob}
}
