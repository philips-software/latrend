#' @include method.R
#' @include methodLcmmGMM.R

#' @name interface-lcmm
#' @rdname interface-lcmm
#' @title lcmm interface
#' @seealso [lcMethodLcmmGBTM] [lcMethodLcmmGMM] \link[lcmm]{lcmm-package}
#' @keywords internal
NULL

setClass('lcMethodLcmmGBTM', contains = 'lcMethod')

setValidity('lcMethodLcmmGBTM', function(object) {
  assert_that(
    !has_name(object, 'random'),
    msg = 'formula cannot contain random effects. Consider using lcMethodLcmmGMM.')

  if (isArgDefined(object, 'classmb')) {
    assert_that(!hasResponse(object$classmb))
  }
})


#' @export
#' @title Specify GBTM method
#' @description Group-based trajectory modeling through fixed-effects modeling.
#' @inheritParams lcMethodLcmmGMM
#' @examples
#' data(latrendData)
#' method <- lcMethodLcmmGBTM(fixed = Y ~ Time, mixture = ~ 1,
#'    id = "Id", time = "Time", nClusters = 3)
#' gbtm <- latrend(method, data = latrendData)
#' summary(gbtm)
#'
#' method <- lcMethodLcmmGBTM(fixed = Y ~ Time, mixture = ~ Time,
#'     id = "Id", time = "Time", nClusters = 3)
#' @family lcMethod implementations
#' @references
#' \insertRef{proustlima2017estimation}{latrend}
#'
#' \insertRef{proustlima2019lcmm}{latrend}
lcMethodLcmmGBTM = function(fixed,
                            mixture = ~ 1,
                            classmb =  ~ 1,
                            time = getOption('latrend.time'),
                            id = getOption('latrend.id'),
                            nClusters = 2,
                            ...) {
  lcMethod.call(
    'lcMethodLcmmGBTM',
    call = match.call.all(),
    defaults = lcmm::hlme,
    excludeArgs = c(
      'data',
      'subject',
      'returndata',
      'ng',
      'verbose'
    )
  )
}

#' @rdname interface-lcmm
#' @inheritParams getName
setMethod('getName', signature('lcMethodLcmmGBTM'), function(object) 'group-based trajectory modeling using lcmm')

#' @rdname interface-lcmm
setMethod('getShortName', signature('lcMethodLcmmGBTM'), function(object) 'gbtm')

#' @rdname interface-lcmm
setMethod('preFit', signature('lcMethodLcmmGBTM'), gmm_prepare)

#' @rdname interface-lcmm
#' @inheritParams fit
setMethod('fit', signature('lcMethodLcmmGBTM'), function(method, data, envir, verbose, ...) {
  model = gmm_fit(method, data, envir, verbose, ...)

  new(
    'lcModelLcmmGBTM',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})

#' @rdname interface-lcmm
setMethod('responseVariable', signature('lcMethodLcmmGBTM'), function(object, ...) {
  getResponse(object$fixed)
})
