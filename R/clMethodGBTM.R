#' @include clMethod.R
#' @include clMethodGMM.R
setClass('clMethodGBTM', contains='clMethod')


#' @export
#' @title Specify GBTM method
#' @description Group-based trajectory modeling through fixed-effects modeling.
#' @inheritParams clMethodKML
#' @inheritParams clMethodGMM
#' @param formula Formula of form Response ~ Var1 + CLUSTER * Var2 + .
#' Variables specified in the model are included as fixed effects.
#' If an interaction is specified with the CLUSTER term then these covariates are included as fixed and mixture effects.
#' @inheritParams lcmm::hlme
#' @examples
#' method = clMethodGBTM(Value ~ CLUSTER,
#'                      time='Assessment',
#'                      id='Subject', nClusters=3)
#' gmm = cluslong(method, data=testLongData)
#' summary(gmm)
#' @family clMethod classes
clMethodGBTM = function(formula=Value ~ 1 + CLUSTER,
                       formula.mb=~1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       maxIter=500,
                       idiag=FALSE,
                       nwg=FALSE,
                       cor=NULL,
                       convB=1e-4,
                       convL=1e-4,
                       convG=1e-4) {
  object = new('clMethodGBTM', call=match.call.defaults())

  if(getOption('cluslong.checkArgs')) {
    checkArgs(object, envir=parent.frame())
  }

  return(object)
}

setMethod('checkArgs', signature('clMethodGBTM'), function(object, envir) {
  environment(object) = envir
  assert_that(all(formalArgs(clMethodGBTM) %in% names(getCall(object))), msg='clMethod object is missing required arguments')

  if(isArgDefined(object, 'formula')) {
    f = formula(object)
    assert_that(hasSingleResponse(object$formula))

    reTerms = getREterms(f)
    assert_that(length(getREterms(f)) == 0, msg='formula cannot contain random effects. Consider using clMethodGMM.')
  }

  if(isArgDefined(object, 'formula.mb')) {
    assert_that(!hasResponse(formula(object, 'mb')))
  }

  if(isArgDefined(object, 'nClusters')) {
    assert_that(is.count(object$nClusters))
  }

  if(isArgDefined(object, 'maxIter')) {
    assert_that(is.count(object$maxIter))
  }
})


setMethod('getName', signature('clMethodGBTM'), function(object) 'group-based trajectory modeling')

setMethod('getName0', signature('clMethodGBTM'), function(object) 'gbtm')

setMethod('prepare', signature('clMethodGBTM'), gmm_prepare)

setMethod('fit', signature('clMethodGBTM'), gmm_fit)

setMethod('finalize', signature('clMethodGBTM'), function(method, data, envir, verbose, ...) {
  model = new('clModelGBTM',
              method=method,
              data=data,
              model=envir$model,
              clusterNames=make.clusterNames(method$nClusters))
  return(model)
})