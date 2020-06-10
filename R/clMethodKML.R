#' @include clMethod.R
setClass('clMethodKML', contains='clMatrixMethod')

#' @export
#' @import kml
#' @import longitudinalData
#' @inheritDotParams kml::kml
#' @inheritDotParams kml::parALGO
#' @title Specify a longitudinal k-means (KML) method
#' @param formula Formula used to specify the response variable to model. On the right-hand side, only `~ 0` is supported.
#' @param time Time variable.
#' @param id Strata variable.
#' @param nClusters Number of clusters.
#' @examples
#' method = clMethodKML(Value ~ 0, nClusters=3)
#' model = cluslong(method, testLongData)
#' @family clMethod implementations
clMethodKML = function(response=getOption('cluslong.response'),
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       ...
) {
  m = .clMethod('clMethodKML', call=match.call.defaults(),
           defaults=c(kml::kml, kml::parALGO),
           excludeArgs=c('object', 'nbClusters', 'parAlgo', 'toPlot', 'saveFreq'))

  # fix for meanNA conflict
  if(has_name(m, 'centerMethod') && as.character(m[['centerMethod', eval=FALSE]]) == 'meanNA') {
    m = update(m, centerMethod=longitudinalData::meanNA)
  }
  return(m)
}

setMethod('getName', signature('clMethodKML'), function(object) 'longitudinal k-means (KML)')

setMethod('getShortName', signature('clMethodKML'), function(object) 'kml')


setMethod('preFit', signature('clMethodKML'), function(method, data, envir, verbose, ...) {
  e = callNextMethod()

  valueColumn = responseVariable(method)

  # Model specification
  cat(verbose, 'Creating clusterLongData object...', level=verboseLevels$finest)

  parRefArgs = list(saveFreq = 1e99, scale=FALSE)
  parArgs = modifyList(parRefArgs, as.list(method, args=kml::parALGO), keep.null=TRUE)
  e$par = do.call(kml::parALGO, parArgs)

  e$cld = clusterLongData(traj=e$dataMat, idAll=rownames(e$dataMat), time=sort(unique(data[[timeVariable(method)]])))
  return(e)
})


setMethod('fit', signature('clMethodKML'), function(method, data, envir, verbose, ...) {
  cld = envir$cld

  # Helper variables
  valueColumn = responseVariable(method)
  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  cat(verbose, 'Running kml()...', level=verboseLevels$finest)
  suppressFun(
    # note that slowKML throws an error for nbClusters=1
    kml::kml(cld, nbClusters=method$nClusters, nbRedrawing=method$nbRedrawing, toPlot='none', parAlgo=envir$par)
  )

  new('clModelKML',
      method=method,
      data=data,
      model=cld,
      clusterNames=make.clusterNames(method$nClusters))
})

