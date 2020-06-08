#' @include clMethodTwoStep.R
setClass('clMethodGCKM', contains='clMethod')

setValidity('clMethodGCKM', function(object) {
  assert_that(has_clMethod_args(object, formalArgs(clMethodGCKM)))

  if(isArgDefined(object, 'formula')) {
    assert_that(hasSingleResponse(object$formula))
  }
})

#' @export
#' @importFrom lme4 lmer lmerControl ranef
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
#' @family clMethod implementations
clMethodGCKM = function(formula=Value ~ 1,
                        time=getOption('cluslong.time'),
                        id=getOption('cluslong.id'),
                        nClusters=2,
                        center=meanNA,
                        ...

) {
  .clMethod('clMethodGCKM', call=match.call.defaults(),
           defaults=c(lmer, kmeans),
           excludeArgs=c('data', 'centers', 'trace'))
}

setMethod('getName', signature('clMethodGCKM'), function(object) 'two-step using LME and k-means')

setMethod('getShortName', signature('clMethodGCKM'), function(object) 'gckm')

clMethodGCKM_as_twostep = function(method) {
  call = getCall(method)
  call$response = getResponse(method$formula)
  call$representationStep = representationStepGCKM
  call$clusterStep = clusterStepGCKM
  call$standardize = scale
  new('clMethodTwoStep', call=call)
}

setMethod('prefit', signature('clMethodGCKM'), function(method, data, envir, verbose) {
  method = clMethodGCKM_as_twostep(method)
  prefit(method, data=data, envir=envir, verbose=verbose)
})

setMethod('fit', signature('clMethodGCKM'), function(method, data, envir, verbose, ...) {
  method = clMethodGCKM_as_twostep(method)
  fit(method, data=data, envir=envir, verbose=verbose, ...)
})



representationStepGCKM = function(method, data, verbose, ...) {
  cat(verbose, 'Representation step...')
  fixedStr = deparse(method$formula)
  randomStr = dropResponse(method$formula) %>%
    deparse %>%
    substring(2)
  lmmFormula = paste0(fixedStr, ' + (', randomStr, '|', method$id, ')') %>% as.formula(env=NULL)

  lmm = lmer(formula=lmmFormula, data=data, REML=method$REML, control=method$control, verbose=canShow(verbose, 'fine'))

  e = new.env()
  e$model = lmm
  e$repMat = ranef(lmm)[[method$id]] %>% as.matrix
  return(e)
}

clusterStepGCKM = function(method, data, repMat, envir, verbose, ...) {
  cat(verbose, 'Cluster step...')
  km = kmeans(repMat, centers=method$nClusters, trace=canShow(verbose, 'fine'))

  clModelCustom(method=method,
                data=data, clusterAssignments=km$cluster,
                clusterTrajectories=method$center,
                model=km,
                converged=!km$ifault)
}