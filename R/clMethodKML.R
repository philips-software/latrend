#' @include clMethod.R
setClass('clMethodKML', contains='clMatrixMethod')

#' @export
#' @import kml
#' @import longitudinalData
#' @inheritDotParams kml::kml
#' @inheritDotParams kml::parALGO
#' @title Specify KML method
#' @param formula Formula used to specify the response variable to model. On the right-hand side, only `~ 0` is supported.
#' @param time Time variable.
#' @param id Strata variable.
#' @param nClusters Number of clusters.
#' @examples
#' method = clMethodKML(Value ~ 0, nClusters=3)
#' model = cluslong(method, testLongData)
#' @family clMethod implementations
clMethodKML = function(formula=Value ~ 0,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       ...
) {
  clMethod('clMethodKML', call=match.call.defaults(),
           defaults=c(kml::kml, kml::parALGO),
           excludeArgs=c('object', 'nbClusters', 'parAlgo', 'toPlot', 'saveFreq'))
}

setMethod('getName', signature('clMethodKML'), function(object) 'longitudinal k-means (KML)')

setMethod('getName0', signature('clMethodKML'), function(object) 'kml')


setMethod('prepare', signature('clMethodKML'), function(method, data, verbose, ...) {
  e = callNextMethod()

  assert_that(hasSingleResponse(method$formula))
  assert_that(!hasCovariates(method$formula), msg='covariates are not supported')

  valueColumn = formula(method) %>%
    getResponse()

  # Model specification
  cat(verbose, 'Creating clusterLongData object...', level=verboseLevels$finest)

  parRefArgs = list(saveFreq = 1e99, scale=FALSE)
  parArgs = modifyList(parRefArgs, as.list(method, fun=parALGO), keep.null=TRUE)
  e$par = do.call(parALGO, parArgs)

  e$cld = clusterLongData(traj=e$dataMat, idAll=rownames(e$dataMat), time=sort(unique(data[[method$time]])))
  return(e)
})


setMethod('fit', signature('clMethodKML'), function(method, data, envir, verbose, ...) {
  e = new.env(parent=envir)

  cld = envir$cld
  # Helper variables
  valueColumn = formula(method) %>% getResponse()
  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  cat(verbose, 'Running kml()...', level=verboseLevels$finest)
  suppressFun(
    kml(cld, nbClusters=method$nClusters, nbRedrawing=method$nbRedrawing, toPlot='none', parAlgo=envir$par)
  )
  e$cld = cld
  return(e)
})


setMethod('finalize', signature('clMethodKML'), function(method, data, envir, verbose, ...) {
  model = new('clModelKML',
              method=method,
              data=data,
              model=envir$cld,
              clusterNames=make.clusterNames(method$nClusters))
  return(model)
})
