#' @include methodMatrix.R

#' @name interface-crimCV
#' @rdname interface-crimCV
#' @title crimCV interface
#' @seealso [lcMethodCrimCV] \link[crimCV]{crimCV}
#' @keywords internal
NULL

setClass('lcMethodCrimCV', contains = 'lcMatrixMethod')

#' @export
#' @title Specify a zero-inflated repeated-measures GBTM method
#' @inheritParams lcMatrixMethod-class
#' @inheritParams lcMethodKML
#' @param ... Arguments passed to [crimCV::crimCV].
#' The following external arguments are ignored: Dat, ng.
#' @examples
#' \donttest{
#' library(crimCV)
#' data(latrendData)
#' method <- lcMethodCrimCV("Y", id = "Id", time = "Time", nClusters = 3, dpolyp = 1, init = 2)
#' model <- latrend(method, data = subset(latrendData, Time > .5))
#' plot(model)
#'
#' data(TO1adj)
#' method <- lcMethodCrimCV(response = "Offenses", time = "Offense", id = "Subject",
#'   nClusters = 2, dpolyp = 1, init = 2)
#' model <- latrend(method, data = TO1adj[1:100, ])
#' }
#' @family lcMethod implementations
#' @references
#' \insertRef{nielsen2018crimcv}{latrend}
lcMethodCrimCV = function(response,
                          time = getOption('latrend.time'),
                          id = getOption('latrend.id'),
                          nClusters = 2,
                          ...) {
  lcMethod.call(
    'lcMethodCrimCV',
    call = match.call.defaults(),
    defaults = crimCV::crimCV,
    excludeArgs = c('Dat', 'ng')
  )
}

#' @rdname interface-crimCV
#' @inheritParams getName
setMethod('getName', signature('lcMethodCrimCV'), function(object) 'zero-inflated GBTM using crimcv')

#' @rdname interface-crimCV
setMethod('getShortName', signature('lcMethodCrimCV'), function(object) 'crimcv')

#' @rdname interface-crimCV
setMethod('prepareData', signature('lcMethodCrimCV'), function(method, data, verbose, ...) {
  times = sort(unique(data[[timeVariable(method)]]))

  refTimes = seq(first(times), last(times), length.out = length(times))
  assert_that(isTRUE(all.equal(times, refTimes)), msg = 'Measurement times must be equally spaced, starting from 0 and ending at 1. This requirement is enforced because crimCV internally specifies the measurement times in this way.')

  callNextMethod()
})

#' @rdname interface-crimCV
#' @inheritParams fit
setMethod('fit', signature('lcMethodCrimCV'), function(method, data, envir, verbose, ...) {
  suppressFun = ifelse(as.logical(verbose), force, capture.output)
  time = timeVariable(method)

  args = as.list(method, args = crimCV::crimCV)
  args$Dat = envir$dataMat
  args$ng = method$nClusters

  suppressFun({
    model = do.call(crimCV::crimCV, args)
  })
  model$data = envir$dataMat
  model$minTime = min(data[[time]])
  model$durTime = max(data[[time]]) - model$minTime

  assert_that(has_name(model, 'beta'), msg = 'invalid crimCV model returned from fit. The model either failed to initialize, converge, or its specification is unsupported.')

  new(
    'lcModelCrimCV',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
