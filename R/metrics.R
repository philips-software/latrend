#' @name latrend-metrics
#' @title Metrics
#' @description The package supports a variety of metrics that help to evaluate and compare [estimated models][lcModel].
#'
#' * [Internal metrics][metric]: metrics that assess the adequacy of the model with respect to the data.
#' * [External metrics][externalMetric]: metrics that compare two models.
#'
#' Users can implement new metrics through [defineInternalMetric()] and [defineExternalMetric()].
#' Custom-defined metrics are accessible using the same by-name mechanism as the other metrics.
#'
#' @inheritSection metric Supported internal metrics
#' @inheritSection externalMetric Supported external metrics
#' @seealso [metric] [externalMetric]
NULL
