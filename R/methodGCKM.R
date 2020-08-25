#' @include methodTwoStep.R
setClass('lcMethodGCKM', contains = 'lcMethod')

setValidity('lcMethodGCKM', function(object) {
  assert_that(has_lcMethod_args(object, formalArgs(lcMethodGCKM)))

  if (isArgDefined(object, 'formula')) {
    assert_that(hasSingleResponse(object$formula))
  }
})

#' @export
#' @title Two-step clustering through linear mixed modeling and k-means
#' @description Two-step clustering through linear mixed modeling and k-means.
#' @inheritParams lcMethodTwoStep
#' @param formula Formula, including a random effects component for the trajectory. See [lme4::lmer] formula syntax.
#' @param time The name of the time variable..
#' @param id The name of the trajectory identifier variable.
#' @param nClusters The number of clusters.
#' @param ... Arguments passed to [lme4::lmer].
#' The following external arguments are ignored: data, centers, trace.
#' @examples
#' library(lme4)
#' data(latrendData)
#' method <- lcMethodGCKM(Y ~ Time + (Time | Id), id = "Id", time = "Time", nClusters = 3)
#' model <- latrend(method, latrendData)
#' @family lcMethod implementations
lcMethodGCKM = function(formula,
                        time = getOption('latrend.time'),
                        id = getOption('latrend.id'),
                        nClusters = 2,
                        center = meanNA,
                        ...) {
  .loadOptionalPackage('lme4')

  lcMethod.call(
    'lcMethodGCKM',
    call = match.call.defaults(),
    defaults = c(lme4::lmer, kmeans),
    excludeArgs = c('data', 'centers', 'trace')
  )
}

#' @rdname interface-featureBased
setMethod('getName', signature('lcMethodGCKM'), function(object) 'two-step using LME and k-means')

#' @rdname interface-featureBased
setMethod('getShortName', signature('lcMethodGCKM'), function(object) 'gckm')

lcMethodGCKM_as_twostep = function(method) {
  call = getCall(method)
  call$response = getResponse(method$formula)
  call$representationStep = representationStepGCKM
  call$clusterStep = clusterStepGCKM
  call$standardize = scale
  lcMethod.call('lcMethodTwoStep', call = call)
}

#' @rdname interface-featureBased
setMethod('compose', signature('lcMethodGCKM'), function(method, envir = NULL) {
  evaluate.lcMethod(method, try = TRUE, envir = envir)
})

#' @rdname interface-featureBased
setMethod('preFit', signature('lcMethodGCKM'), function(method, data, envir, verbose) {
  method = lcMethodGCKM_as_twostep(method)
  preFit(method,
         data = data,
         envir = envir,
         verbose = verbose)
})

#' @rdname interface-featureBased
setMethod('fit', signature('lcMethodGCKM'), function(method, data, envir, verbose, ...) {
  method = lcMethodGCKM_as_twostep(method)
  fit(method,
      data = data,
      envir = envir,
      verbose = verbose,
      ...)
})



representationStepGCKM = function(method, data, verbose, ...) {
  cat(verbose, 'Representation step...')
  fixedStr = deparse(method$formula)
  randomStr = dropResponse(method$formula) %>%
    deparse %>%
    substring(2)
  lmmFormula = paste0(fixedStr, ' + (', randomStr, '|', idVariable(method), ')') %>% as.formula(env =
                                                                                                  NULL)
  lmm = lme4::lmer(
    formula = lmmFormula,
    data = data,
    REML = method$REML,
    control = method$control,
    verbose = canShow(verbose, 'fine')
  )

  e = new.env()
  e$model = lmm
  e$repMat = lme4::ranef(lmm)[[idVariable(method)]] %>% as.matrix()
  return(e)
}

clusterStepGCKM = function(method, data, repMat, envir, verbose, ...) {
  cat(verbose, 'Cluster step...')
  km = kmeans(repMat,
              centers = method$nClusters,
              trace = canShow(verbose, 'fine'))

  lcModelCustom(
    method = method,
    response = responseVariable(method),
    data = data,
    clusterAssignments = km$cluster,
    clusterTrajectories = method$center,
    model = km,
    converged = !km$ifault
  )
}
