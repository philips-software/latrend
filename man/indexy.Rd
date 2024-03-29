% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method.R
\name{[[,lcMethod-method}
\alias{[[,lcMethod-method}
\alias{$,lcMethod-method}
\title{Retrieve and evaluate a lcMethod argument by name}
\usage{
\S4method{$}{lcMethod}(x, name)

\S4method{[[}{lcMethod}(x, i, eval = TRUE, envir = NULL)
}
\arguments{
\item{x}{The \code{lcMethod} object.}

\item{name}{The argument name, as \code{character}.}

\item{i}{Name or index of the argument to retrieve.}

\item{eval}{Whether to evaluate the call argument (enabled by default).}

\item{envir}{The \code{environment} in which to evaluate the argument. This argument is only applicable when \code{eval = TRUE}.}
}
\value{
The argument \code{call} or evaluation result.
}
\description{
Retrieve and evaluate a lcMethod argument by name
}
\examples{
method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
method$nClusters # 3
m = lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 5)
m[["nClusters"]] # 5

k = 2
m = lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = k)
m[["nClusters", eval=FALSE]] # k
}
\seealso{
Other lcMethod functions: 
\code{\link{as.data.frame.lcMethod}()},
\code{\link{as.data.frame.lcMethods}()},
\code{\link{as.lcMethods}()},
\code{\link{as.list.lcMethod}()},
\code{\link{evaluate.lcMethod}()},
\code{\link{formula.lcMethod}()},
\code{\link{lcMethod-class}},
\code{\link{names,lcMethod-method}},
\code{\link{update.lcMethod}()}
}
\concept{lcMethod functions}
