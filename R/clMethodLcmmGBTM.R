#' @include clMethod.R
#' @include clMethodLcmmGMM.R
setClass('clMethodLcmmGBTM', contains='clMethod')

setValidity('clMethodLcmmGBTM', function(object) {
  if(isArgDefined(object, 'formula')) {
    f = formula(object)
    assert_that(hasSingleResponse(object$formula))

    reTerms = getREterms(f)
    assert_that(length(getREterms(f)) == 0, msg='formula cannot contain random effects. Consider using clMethodLcmmGMM.')
  }

  if(isArgDefined(object, 'formula.mb')) {
    assert_that(!hasResponse(formula(object, 'mb')))
  }
})


#' @export
#' @title Specify GBTM method
#' @description Group-based trajectory modeling through fixed-effects modeling.
#' @inheritParams clMethodLcmmGMM
#' @param formula Formula of form Response ~ Var1 + CLUSTER * Var2 + .
#' Variables specified in the model are included as fixed effects.
#' If an interaction is specified with the CLUSTER term then these covariates are included as fixed and mixture effects.
#' @inheritDotParams lcmm::lcmm
#' @examples
#' method = clMethodLcmmGBTM(Value ~ CLUSTER,
#'                      time='Time',
#'                      id='Id', nClusters=3)
#' gmm = cluslong(method, data=testLongData)
#' summary(gmm)
#' @family clMethod implementations
clMethodLcmmGBTM = function(formula=Value ~ 1 + CLUSTER,
                       formula.mb=~1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       ...
) {
  .clMethod.call('clMethodLcmmGBTM', call=match.call.defaults(),
           defaults=lcmm::lcmm,
           excludeArgs=c('data', 'fixed', 'random', 'mixture', 'subject', 'classmb', 'returndata', 'ng', 'verbose'))
}


setMethod('getName', signature('clMethodLcmmGBTM'), function(object) 'group-based trajectory modeling using lcmm')

setMethod('getShortName', signature('clMethodLcmmGBTM'), function(object) 'gbtm')

setMethod('preFit', signature('clMethodLcmmGBTM'), gmm_prepare)

setMethod('fit', signature('clMethodLcmmGBTM'), function(method, data, envir, verbose, ...) {
  model = gmm_fit(method, data, envir, verbose, ...)

  new('clModelLcmmGBTM',
      method=method,
      data=data,
      model=model,
      clusterNames=make.clusterNames(method$nClusters))
})
