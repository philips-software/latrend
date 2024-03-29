% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{PAP.adh1y}
\alias{PAP.adh1y}
\title{Biweekly Mean PAP Therapy Adherence of OSA Patients over 1 Year}
\format{
A \code{data.frame} comprising longitudinal data of 500 patients, each having 26 observations over a period of 1 year.
Each row represents a patient observation interval (two weeks), with columns:
\describe{
\item{Patient}{\code{factor}: The patient identifier, where each level represents a simulated patient.}
\item{Biweek}{\code{integer}: Two-week interval index. Starts from 1.}
\item{MaxDay}{\code{integer}: The last day used for the aggregation of the respective interval, \code{integer}}
\item{UsageHours}{\code{numeric}: The mean hours of usage in the respective week.
Greater than or equal to zero, and typically around 4-6 hours.}
\item{Group}{\code{factor}: The reference group (i.e., adherence pattern) from which this patient was generated.}
}
}
\source{
This dataset was generated based on the cluster-specific descriptive statistics table
provided in Aloia et al. (2008),
with some adjustments made in order to improve cluster separation for demonstration purposes.

\insertRef{aloia2008time}{latrend}
}
\usage{
PAP.adh1y
}
\description{
A simulated longitudinal dataset comprising 500 patients with obstructive sleep apnea (OSA)
during their first year on CPAP therapy.
The dataset contains the patient usage hours, averaged over 2-week periods.

The daily usage data underlying the downsampled dataset was simulated based on 7 different adherence patterns.
The defined adherence patterns were inspired by the adherence patterns identified by Aloia et al. (2008),
with slight adjustments
}
\note{
This dataset is only intended for demonstration purposes.
While the data format will remain the same, the data content is subject to change in future versions.
}
\examples{
data(PAP.adh1y)

if (require("ggplot2")) {
  plotTrajectories(PAP.adh1y, id = "Patient", time = "Biweek", response = "UsageHours")

  # plot according to cluster ground truth
  plotTrajectories(
    PAP.adh1y,
    id = "Patient",
    time = "Biweek",
    response = "UsageHours",
    cluster = "Group"
  )
}
}
\seealso{
\link{latrend-data}
}
\keyword{datasets}
