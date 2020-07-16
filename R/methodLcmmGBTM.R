#' @include method.R
#' @include methodLcmmGMM.R
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
#' @param formula Formula of form Response ~ Var1 + CLUSTER * Var2 + .
#' Variables specified in the model are included as fixed effects.
#' If an interaction is specified with the CLUSTER term then these covariates are included as fixed and mixture effects.
#' @inheritDotParams lcmm::lcmm
#' @examples
#' method = lcMethodLcmmGBTM(Value ~ CLUSTER,
#'                      time='Time',
#'                      id='Id', nClusters=3)
#' gmm = latrend(method, data=testLongData)
#' summary(gmm)
#' @family lcMethod implementations
lcMethodLcmmGBTM = function(formula = Value ~ 1 + CLUSTER,
                            formula.mb =  ~ 1,
                            time = getOption('latrend.time'),
                            id = getOption('latrend.id'),
                            nClusters = 2,
                            ...) {
  lcMethod.call(
    'lcMethodLcmmGBTM',
    call = match.call.defaults(),
    defaults = lcmm::lcmm,
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


setMethod('getName', signature('lcMethodLcmmGBTM'), function(object) 'group-based trajectory modeling using lcmm')

setMethod('getShortName', signature('lcMethodLcmmGBTM'), function(object) 'gbtm')

setMethod('preFit', signature('lcMethodLcmmGBTM'), gmm_prepare)

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
