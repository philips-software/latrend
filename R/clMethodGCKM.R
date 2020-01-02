#' @include clMethodTwoStep.R
setClass('clMethodGCKM', contains='clMethodTwoStep')

setValidity('clMethodGCKM', function(object) {
  call = getCall(object)
  assert_that(hasSingleResponse(object$formula))

  assert_that(all(formalArgs(clMethodGCKM) %in% names(call)), msg='clMethod object is missing required arguments')

  assert_that(is.count(object$nClusters))
})

#' @export
#' @importFrom lme4 lmer lmerControl
#' @title Two-step clustering through linear mixed modeling and k-means
#' @description Two-step clustering through linear mixed modeling and k-means.
#' @param formula Formula, including a random effects component for the strata.
#' @param time Time variable.
#' @param id Strata variable.
#' @param nClusters Number of clusters.
#' @inheritParams lme4::lmer
#' @inheritParams clMethodTwoStep
#' @examples
#' method = clMethodGCKM(Measurement ~ Assessment + (Assessment | Subject),
#'                      time='Assessment',
#'                      id='Subject', nClusters=3)
clMethodGCKM = function(formula=Value ~ 1,
                        time=getOption('cluslong.time'),
                        id=getOption('cluslong.id'),
                        center=meanNA,
                        REML=FALSE,
                        control=lmerControl(),
                        nClusters=2) {
  call = match.call.defaults()
  object = new('clMethodGCKM', call=call)

  if(getOption('cluslong.checkArgs')) {
    checkArgs(object)
  }
  return(object)
}

setMethod('checkArgs', signature('clMethodGCKM'), function(object, envir) {
  environment(object) = envir
  assert_that(all(formalArgs(clMethodGCKM) %in% names(getCall(object))), msg='clMethod object is missing required arguments')

  if(isArgDefined(object, 'formula')) {
    assert_that(is.formula(object$formula))
    assert_that(hasSingleResponse(object$formula))
  }

  assert_that(!isArgDefined(object, 'nClusters') || is.count(object$nClusters))
})


setMethod('getName', signature('clMethodGCKM'), function(object) 'two-step using LME and k-means')

setMethod('getName0', signature('clMethodGCKM'), function(object) 'gckm')


setMethod('prepare', signature('clMethodGCKM'), function(method, data) {
  method = clMethodGCKM_as_twostep(method)
  callNextMethod()
})

setMethod('fit', signature('clMethodGCKM'), function(method, data, prepEnv) {
  method = clMethodGCKM_as_twostep(method)
  callNextMethod()
})

clMethodGCKM_as_twostep = function(method) {
  call = getCall(method)
  call$response = getResponse(method$formula)
  call$representationStep = representationStepGCKM
  call$clusterStep = clusterStepGCKM
  new('clMethodTwoStep', call=call)
}

representationStepGCKM = function(method, data) {
  loginfo('Representation step...')
  fixedStr = deparse(method$formula)
  randomStr = dropResponse(method$formula) %>%
    deparse %>%
    substring(2)
  lmmFormula = paste0(fixedStr, ' + (', randomStr, '|', method$id, ')') %>% as.formula(env=NULL)

  lmm = lmer(formula=lmmFormula, data=data, REML=method$REML, control=method$control, verbose=canShowModelOutput('FINE'))

  e = new.env()
  e$model = lmm
  e$repMat = ranef(lmm)[[method$id]] %>% as.matrix
  return(e)
}

clusterStepGCKM = function(method, data, repMat, envir) {
  loginfo('Cluster step...')
  km = kmeans(repMat, centers=method$nClusters, trace=canShowModelOutput('FINE'))

  clModelCustom(method=method,
                data=data, clusterAssignments=km$cluster,
                clusterTrajectories=method$center,
                model=km,
                converged=!km$ifault)
}