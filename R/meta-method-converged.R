#' @include meta-method.R

#' @export
#' @rdname lcMetaMethods
#' @examples
#' data(latrendData)
#' method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 2)
#' metaMethod <- lcMetaConverged(method, maxRep = 10)
#' metaMethod
#' model <- latrend(metaMethod, latrendData)
setClass('lcMetaConverged', contains = 'lcMetaMethod')

#' @export
#' @rdname lcMetaMethods
#' @param method The `lcMethod` to use for fitting.
#' @param maxRep The maximum number of fit attempts
lcMetaConverged = function(method, maxRep = Inf) {
  mc = match.call.all()
  mc$method = getCall(method)
  mc$Class = 'lcMetaConverged'
  do.call(new, as.list(mc))
}


#' @rdname lcMetaMethod-interface
setMethod('fit', 'lcMetaConverged', function(method, data, envir, verbose) {
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

      if (is.infinite(method$maxRep)) {
        cat(verbose, sprintf('Method failed to converge. Retrying... attempt %d', attempt))
      } else {
        cat(verbose, sprintf('Method failed to converge. Retrying... attempt %d / %d', attempt, method$maxRep))
      }
    }
  }
})
