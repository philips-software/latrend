#' @include meta-fit.R

#' @export
#' @rdname lcFitMethods
#' @examples
#'
#' data(latrendData)
#' method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time", nClusters = 2)
#' repMethod <- lcFitRep(method, rep = 10, metric = "RSS", maximize = FALSE)
#' repMethod
#' model <- latrend(repMethod, latrendData)
#'
#' minMethod <- lcFitRepMin(method, rep = 10, metric = "RSS")
#'
#' maxMethod <- lcFitRepMax(method, rep = 10, metric = "ASW")
setClass('lcFitRep', contains = 'lcMetaMethod')

#' @export
#' @rdname lcFitMethods
#' @param rep The number of fits
#' @param metric The internal metric to assess the fit.
#' @param maximize Whether to maximize the metric. Otherwise, it is minimized.
lcFitRep = function(method, rep = 10, metric, maximize) {
  mc = match.call.all()
  mc$method = NULL
  mc$Class = 'lcFitRep'

  object = do.call(new, as.list(mc))
  object@method = method
  object
}

#' @export
#' @rdname lcFitMethods
lcFitRepMin = function(method, rep = 10, metric) {
  mc = match.call.all()
  mc$method = NULL
  mc$maximize = FALSE
  mc$Class = 'lcFitRep'

  object = do.call(new, as.list(mc))
  object@method = method
  object
}

#' @export
#' @rdname lcFitMethods
lcFitRepMax = function(method, rep = 10, metric) {
  mc = match.call.all()
  mc$method = NULL
  mc$maximize = TRUE
  mc$Class = 'lcFitRep'

  object = do.call(new, as.list(mc))
  object@method = method
  object
}


#' @rdname interface-metaMethods
setMethod('fit', 'lcFitRep', function(method, data, envir, verbose) {
  bestModel = NULL
  mult = ifelse(method$maximize, 1, -1)
  bestScore = -Inf

  for (i in seq_len(method$rep)) {
    cat(verbose, sprintf('Repeated fitting %d / %d', i, method$rep))
    enter(verbose, level = verboseLevels$fine, suffix = '')
    newModel = fit(getLcMethod(method), data = data, envir = envir, verbose = verbose)
    newScore = metric(newModel, method$metric)
    exit(verbose, level = verboseLevels$fine, suffix = '')

    if (is.finite(newScore) && newScore * mult > bestScore) {
      cat(
        verbose,
        sprintf('Found improved fit for %s = %g (previous is %g)', method$metric, newScore, mult * bestScore),
        level = verboseLevels$fine
      )
      bestModel = newModel
      bestScore = newScore
    }

    if (has_lcMethod_args(getLcMethod(method), 'seed')) {
      # update seed for the next run
      seed = sample.int(.Machine$integer.max, 1L)
      set.seed(seed)
      # update fit method with new seed
      method@method = update(getLcMethod(method), seed = seed, .eval = TRUE)
    }
  }

  bestModel
})

#' @rdname interface-metaMethods
setMethod('validate', 'lcFitRep', function(method, data, envir = NULL, ...) {
  callNextMethod()

  validate_that(
    has_lcMethod_args(method, c('rep', 'metric', 'maximize')),
    is.count(method$rep),
    is.string(method$metric),
    method$metric %in% getInternalMetricNames(),
    is.flag(method$maximize)
  )
})
