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
  if (isArgDefined(object, 'formula')) {
    f = formula(object)
    assert_that(hasSingleResponse(object$formula))

    reTerms = getREterms(f)
    assert_that(length(getREterms(f)) == 0, msg = 'formula cannot contain random effects. Consider using lcMethodLcmmGMM.')
  }

  if (isArgDefined(object, 'formula.mb')) {
    assert_that(!hasResponse(formula(object, 'mb')))
  }
})


#' @export
#' @title Specify GBTM method
#' @description Group-based trajectory modeling through fixed-effects modeling.
#' @inheritParams lcMethodLcmmGMM
#' @param formula A `formula` of the form `Response ~ Var1 + CLUSTER * Var2 + .`
#' Variables specified in the model are included as fixed effects.
#' If an interaction is specified with the `CLUSTER` term then these covariates are included as fixed and mixture effects.
#' @examples
#' data(latrendData)
#' method <- lcMethodLcmmGBTM(Y ~ CLUSTER, id = "Id", time = "Time", nClusters = 3)
#' gbtm <- latrend(method, data = latrendData)
#' summary(gbtm)
#'
#' method <- lcMethodLcmmGBTM(Y ~ CLUSTER * Time, id = "Id", time = "Time", nClusters = 3)
#' @family lcMethod implementations
lcMethodLcmmGBTM = function(formula,
                            formula.mb =  ~ 1,
                            time = getOption('latrend.time'),
                            id = getOption('latrend.id'),
                            nClusters = 2,
                            ...) {
  lcMethod.call(
    'lcMethodLcmmGBTM',
    call = match.call.defaults(),
    defaults = lcmm::hlme,
    excludeArgs = c(
      'data',
      'fixed',
      'random',
      'mixture',
      'subject',
      'classmb',
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
