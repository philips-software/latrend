#' @include clMatrixMethod.R
setClass('clMethodCrimCV', contains = 'clMatrixMethod')

#' @export
#' @importFrom crimCV crimCV
#' @title Specify a zero-inflated repeated-measures GBTM method
#' @inheritParams clMatrixMethod
#' @inheritDotParams crimCV::crimCV
#' @examples
#' method = clMethodCrimCV(Value ~ 0, nClusters=3)
#' model = cluslong(method, testLongData)
#'
#' library(crimCV)
#' data(TO1adj)
#' method = clMethodCrimCV(Offenses ~ 0, time='Offense', id='Subject')
#' model = cluslong(method, TO1adj)
#' @family clMethod implementations
clMethodCrimCV = function(formula = Value ~ 0,
                          time = getOption('cluslong.time'),
                          id = getOption('cluslong.id'),
                          nClusters = 2,
                          ...) {
  .clMethod.call(
    'clMethodCrimCV',
    call = match.call.defaults(),
    defaults = crimCV::crimCV,
    excludeArgs = c('Dat', 'ng')
  )
}


setMethod('getName', signature('clMethodCrimCV'), function(object) 'zero-inflated GBTM using crimcv')

setMethod('getShortName', signature('clMethodCrimCV'), function(object) 'crimcv')

setMethod('prepareData', signature('clMethodCrimCV'), function(method, data, verbose, ...) {
  times = sort(unique(data[[timeVariable(method)]]))

  refTimes = seq(first(times), last(times), length.out = length(times))
  assert_that(isTRUE(all.equal(times, refTimes)), msg = 'Measurement times must be equally spaced, starting from 0 and ending at 1. This requirement is enforced because crimCV internally specifies the measurement times in this way.')

  callNextMethod()
})

#' @importFrom crimCV crimCV
setMethod('fit', signature('clMethodCrimCV'), function(method, data, envir, verbose, ...) {
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
    'clModelCrimCV',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
