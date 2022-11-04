#' @include meta-method.R

#' @export
#' @name lcFitMethods
#' @rdname lcFitMethods
#' @title Method fit modifiers
#' @description A collection of special methods that adapt the fitting procedure of the underlying longitudinal cluster method.
#' Supported fit methods:
#' * `lcFitConverged`: Fit a method until a converged result is obtained.
#' @examples
#' data(latrendData)
#' method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 2)
#' metaMethod <- lcFitConverged(method, maxRep = 10)
#' metaMethod
#' model <- latrend(metaMethod, latrendData)
setClass('lcFitConverged', contains = 'lcMetaMethod')

#' @export
#' @rdname lcFitMethods
#' @param method The `lcMethod` to use for fitting.
#' @param maxRep The maximum number of fit attempts
lcFitConverged = function(method, maxRep = Inf) {
  mc = match.call.all()
  mc$method = getCall(method)
  mc$Class = 'lcFitConverged'
  do.call(new, as.list(mc))
}


#' @rdname interface-metaMethods
setMethod('fit', 'lcFitConverged', function(method, data, envir, verbose) {
  attempt = 1L

  repeat {
    enter(verbose, level = verboseLevels$fine, suffix = '')
    model = fit(getLcMethod(method), data = data, envir = envir, verbose = verbose)
    exit(verbose, level = verboseLevels$fine, suffix = '')

    if (converged(model)) {
      return (model)
    } else if (attempt >= method$maxRep) {
      warning(
        sprintf(
          'Failed to obtain converged result for %s within %d attempts.\n\tReturning last model.',
          class(getLcMethod(method))[1],
          method$maxRep
        ),
        immediate. = TRUE
      )
      return (model)
    } else {
      attempt = attempt + 1L
      seed = sample.int(.Machine$integer.max, 1L)
      set.seed(seed)
      if (has_lcMethod_args(getLcMethod(method), 'seed')) {
        # update fit method with new seed
        method@arguments$method = update(getLcMethod(method), seed = seed, .eval = TRUE)
      }

      if (is.infinite(method$maxRep)) {
        cat(verbose, sprintf('Method failed to converge. Retrying... attempt %d', attempt))
      } else {
        cat(verbose, sprintf('Method failed to converge. Retrying... attempt %d / %d', attempt, method$maxRep))
      }
    }
  }
})

#' @rdname interface-metaMethods
setMethod('validate', 'lcFitConverged', function(method, data, envir = NULL, ...) {
  callNextMethod()

  validate_that(
    has_lcMethod_args(method, 'maxRep'),
    is.count(method$maxRep)
  )
})
