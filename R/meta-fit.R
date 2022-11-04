#' @include meta-method.R

#' @name lcFitMethods
#' @rdname lcFitMethods
#' @title Method fit modifiers
#' @description A collection of special methods that adapt the fitting procedure of the underlying longitudinal cluster method.
#' Supported fit methods:
#' * `lcFitConverged`: Fit a method until a converged result is obtained.
#' * `lcFitRep`: Repeatedly fit a method and return the best result based on a given internal metric.
#' * `lcFitRepMin`: Repeatedly fit a method and return the best result that minimizes the given internal metric.
#' * `lcFitRepMax`: Repeatedly fit a method and return the best result that maximizes the given internal metric.
NULL
