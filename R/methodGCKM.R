#' @include methodFeature.R methodLMKM.R
setClass('lcMethodGCKM', contains = 'lcMethodLMKM')

#' @export
#' @title Two-step clustering through latent growth curve modeling and k-means
#' @description Two-step clustering through latent growth curve modeling and k-means.
#' @inheritParams lcMethodLMKM
#' @param formula Formula, including a random effects component for the trajectory. See [lme4::lmer] formula syntax.
#' @param time The name of the time variable..
#' @param id The name of the trajectory identifier variable.
#' @param nClusters The number of clusters.
#' @param ... Arguments passed to [lme4::lmer].
#' The following external arguments are ignored: data, centers, trace.
#' @examples
#' data(latrendData)
#'
#' if (require("lme4")) {
#'   method <- lcMethodGCKM(Y ~ (Time | Id), id = "Id", time = "Time", nClusters = 3)
#'   model <- latrend(method, latrendData)
#' }
#' @family lcMethod implementations
lcMethodGCKM = function(
  formula,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  nClusters = 2,
  center = meanNA,
  standardize = scale,
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodGCKM'
  do.call(new, as.list(mc))
}

#' @rdname interface-featureBased
setMethod('getArgumentDefaults', signature('lcMethodGCKM'), function(object) {
  .loadOptionalPackage('lme4')
  c(
    formals(lcMethodGCKM),
    formals(lme4::lmer),
    formals(kmeans),
    callNextMethod()
  )
})

#' @rdname interface-featureBased
setMethod('getArgumentExclusions', signature('lcMethodGCKM'), function(object) {
  union(
    callNextMethod(),
    c('data', 'centers', 'trace')
  )
})

#' @rdname interface-featureBased
setMethod('getName', signature('lcMethodGCKM'), function(object) 'two-step using LME and k-means')

#' @rdname interface-featureBased
setMethod('getShortName', signature('lcMethodGCKM'), function(object) 'gckm')


#' @rdname interface-featureBased
setMethod('prepareData', signature('lcMethodGCKM'), function(method, data, verbose) {
  cat(verbose, 'Representation step...')
  lmm = lme4::lmer(
    formula = method$formula,
    data = data,
    REML = method$REML,
    control = method$control,
    verbose = canShow(verbose, 'fine')
  )

  envir = new.env()
  envir$converged = length(lmm@optinfo$conv$lme4) == 0
  envir$x = lme4::ranef(lmm)[[idVariable(method)]] %>% as.matrix()

  envir
})

