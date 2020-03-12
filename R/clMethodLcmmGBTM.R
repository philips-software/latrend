#' @include clMethod.R
#' @include clMethodLcmmGMM.R
setClass('clMethodLcmmGBTM', contains='clMethod')


#' @export
#' @title Specify GBTM method
#' @description Group-based trajectory modeling through fixed-effects modeling.
#' @inheritParams clMethodKML
#' @inheritParams clMethodLcmmGMM
#' @param formula Formula of form Response ~ Var1 + CLUSTER * Var2 + .
#' Variables specified in the model are included as fixed effects.
#' If an interaction is specified with the CLUSTER term then these covariates are included as fixed and mixture effects.
#' @inheritParams lcmm::lcmm
#' @examples
#' method = clMethodLcmmGBTM(Value ~ CLUSTER,
#'                      time='Time',
#'                      id='Id', nClusters=3)
#' gmm = cluslong(method, data=testLongData)
#' summary(gmm)
#' @family clMethod classes
clMethodLcmmGBTM = function(formula=Value ~ 1 + CLUSTER,
                       formula.mb=~1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       link='linear',
                       intnodes=NULL,
                       idiag=FALSE,
                       nwg=FALSE,
                       cor=NULL,
                       epsY=.5,
                       maxiter=100,
                       nsim=100,
                       range=NULL,
                       partialH=FALSE,
                       convB=1e-4,
                       convL=1e-4,
                       convG=1e-4) {
  object = new('clMethodLcmmGBTM', call=match.call.defaults())

  if(getOption('cluslong.checkArgs')) {
    checkArgs(object, envir=parent.frame())
  }

  return(object)
}

setMethod('checkArgs', signature('clMethodLcmmGBTM'), function(object, envir) {
  environment(object) = envir
  assert_that(all(formalArgs(clMethodLcmmGBTM) %in% names(getCall(object))), msg='clMethod object is missing required arguments')

  if(isArgDefined(object, 'formula')) {
    f = formula(object)
    assert_that(hasSingleResponse(object$formula))

    reTerms = getREterms(f)
    assert_that(length(getREterms(f)) == 0, msg='formula cannot contain random effects. Consider using clMethodLcmmGMM.')
  }

  if(isArgDefined(object, 'formula.mb')) {
    assert_that(!hasResponse(formula(object, 'mb')))
  }

  if(isArgDefined(object, 'nClusters')) {
    assert_that(is.count(object$nClusters))
  }

  if(isArgDefined(object, 'maxiter')) {
    assert_that(is.count(object$maxiter))
  }
})


setMethod('getName', signature('clMethodLcmmGBTM'), function(object) 'group-based trajectory modeling using lcmm')

setMethod('getName0', signature('clMethodLcmmGBTM'), function(object) 'gbtm')

setMethod('prepare', signature('clMethodLcmmGBTM'), gmm_prepare)

setMethod('fit', signature('clMethodLcmmGBTM'), gmm_fit)

setMethod('finalize', signature('clMethodLcmmGBTM'), function(method, data, envir, verbose, ...) {
  model = new('clModelLcmmGBTM',
              method=method,
              data=data,
              model=envir$model,
              clusterNames=make.clusterNames(method$nClusters))
  return(model)
})