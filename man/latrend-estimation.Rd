% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latrend.R
\name{latrend-estimation}
\alias{latrend-estimation}
\title{Overview of \strong{\code{lcMethod}} estimation functions}
\description{
This page presents an overview of the different functions that are available for estimating one or more \link[=lcMethod]{longitudinal cluster methods}.
All functions are prefixed by \emph{"latrend"}.
}
\section{\emph{latrend} estimation functions}{

\itemize{
\item \strong{\code{\link[=latrend]{latrend()}}}: estimate a \link[=lcMethod]{method} on a \link[=latrend-data]{longitudinal dataset}, returning the resulting \link[=lcModel]{model}.
\item \strong{\code{\link[=latrendBatch]{latrendBatch()}}}: estimate multiple \link[=lcMethod]{methods} on multiple \link[=latrend-data]{longitudinal datasets}, returning a \link[=lcModels-class]{list of models}.
\item \strong{\code{\link[=latrendRep]{latrendRep()}}}: repeatedly estimate a \link[=lcMethod]{method} on a \link[=latrend-data]{longitudinal dataset}, returning a \link[=lcModels-class]{list of models}.
\item \strong{\code{\link[=latrendBoot]{latrendBoot()}}}: repeatedly estimate a \link[=lcMethod]{method} on bootstrapped \link[=latrend-data]{longitudinal dataset}, returning a \link[=lcModels-class]{list of models}.
\item \strong{\code{\link[=latrendCV]{latrendCV()}}}: repeatedly estimate a \link[=lcMethod]{method} using cross-validation on a \link[=latrend-data]{longitudinal dataset}, returning a \link[=lcModels-class]{list of models}.
}
}

\section{Parallel estimation}{

The functions involving repeated estimation support parallel computation. \link[=latrend-parallel]{See here.}
}

\seealso{
\link{latrend-package} \link{lcMethod-estimation}
}
