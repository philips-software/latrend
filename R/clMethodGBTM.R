#' @include clMethodGMM.R
setClass('clMethodGBTM', contains='clMethod')

setValidity('clMethodGBTM', function(object) {
  call = getCall(object)
  f = formula(object)
  assert_that(hasSingleResponse(f))
  assert_that(!hasResponse(formula(object, 'mb')))

  assert_that(all(formalArgs(clMethodGBTM) %in% names(call)), msg='clMethodGBTM object is missing required arguments')

  assert_that(length(getREterms(f)) == 0, msg='formula may not contain random effects. Consider using clMethodGMM.')

  assert_that(is.wholeNumber(object$nClusters))
  assert_that(is.wholeNumber(object$maxIter))
})

#' @export
#' @import lcmm
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
  new('clMethodGBTM', call=match.call.defaults())
}

setMethod('getName', signature('clMethodGBTM'), function(object) 'group-based trajectory modeling')

setMethod('getName0', signature('clMethodGBTM'), function(object) 'gbtm')

setMethod('prepare', signature('clMethodGBTM'), gmm_prepare)

setMethod('fit', signature('clMethodGBTM'), gmm_fit)

setMethod('finalize', signature('clMethodGBTM'), function(method, data, control, fitEnv) {
  model = new('clModelGBTM',
              method=method,
              model=fitEnv$model,
              clusterNames=genClusNames(method$nClusters))
  return(model)
})