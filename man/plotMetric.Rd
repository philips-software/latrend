% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{plotMetric}
\alias{plotMetric}
\title{Plot one or more internal metrics for all lcModels}
\usage{
plotMetric(models, name, by = "nClusters", subset, group = character())
}
\arguments{
\item{models}{A \code{lcModels} or list of \code{lcModel} objects to compute and plot the metrics of.}

\item{name}{The name(s) of the metric(s) to compute. If no names are given, the names specified in the \code{latrend.metric} option (WRSS, APPA, AIC, BIC) are used.}

\item{by}{The argument name along which methods are plotted.}

\item{subset}{Logical expression based on the \code{lcModel} method arguments, indicating
which \code{lcModel} objects to keep.}

\item{group}{The argument names to use for determining groups of different models. By default,
all arguments are included.
Specifying \code{group = character()} disables grouping.
Specifying a single argument for grouping uses that specific column as the grouping column.
In all other cases, groupings are represented by a number.}
}
\value{
\code{ggplot2} object.
}
\description{
Plot one or more internal metrics for all lcModels
}
\examples{
data(latrendData)
baseMethod <- lcMethodKML(response = "Y", id = "Id", time = "Time")
methods <- lcMethods(baseMethod, nClusters = 1:6)
models <- latrendBatch(methods, latrendData)
plotMetric(models, c("BIC", "WRSS"))
}
