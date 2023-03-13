#' @include meta-method.R

#' @name lcFitMethods
#' @rdname lcFitMethods
#' @aliases lcMetaMethods
#' @title Method fit modifiers
#' @description A collection of special methods that adapt the fitting procedure of the underlying longitudinal cluster method.
#'
#' NOTE: the underlying implementation is experimental and may change in the future.
#'
#' Supported fit methods:
#' * `lcFitConverged`: Fit a method until a converged result is obtained.
#' * `lcFitRep`: Repeatedly fit a method and return the best result based on a given internal metric.
#' * `lcFitRepMin`: Repeatedly fit a method and return the best result that minimizes the given internal metric.
#' * `lcFitRepMax`: Repeatedly fit a method and return the best result that maximizes the given internal metric.
#' @details
#' Meta methods are immutable and cannot be updated after instantiation. Calling [update()] on a meta method is only used to update arguments of the underlying [lcMethod] object.
NULL
