#' @include clMethod.R
setClass('clMethodKML', contains='clMatrixMethod')

#' @export
#' @import kml
#' @import longitudinalData
#' @inheritParams kml::kml
#' @inheritParams kml::parALGO
#' @title Specify KML method
#' @param formula Formula. Only intercept is supported.
#' @param time Time variable.
#' @param id Strata variable.
#' @param nClusters Number of clusters.
#' @examples
#' method = clMethodKML(Value ~ 1, nClusters=3)
#' model = cluslong(method, testLongData)
#' @family clMethod classes
clMethodKML = function(formula=Value ~ 1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       nbRedrawing=20,
                       maxIt=200,
                       startingCond='kmeans++',
                       imputationMethod='copyMean',
                       distanceName='euclidean',
                       centerMethod=meanNA) {
  object = new('clMethodKML', call=match.call.defaults())

  if(getOption('cluslong.checkArgs')) {
    checkArgs(object, envir=parent.frame())
  }
  return(object)
}


setMethod('checkArgs', signature('clMethodKML'), function(object, envir) {
  environment(object) = envir
  assert_that(all(formalArgs(clMethodKML) %in% names(getCall(object))), msg='clMethod object is missing required arguments')

  if(isArgDefined(object, 'formula')) {
    assert_that(hasSingleResponse(object$formula))
    assert_that(!hasCovariates(object$formula), msg='covariates are not supported')
  }

  if(isArgDefined(object, 'nClusters')) {
    assert_that(is.count(object$nClusters))
  }
})


setMethod('getName', signature('clMethodKML'), function(object) 'longitudinal k-means (KML)')

setMethod('getName0', signature('clMethodKML'), function(object) 'kml')


setMethod('prepare', signature('clMethodKML'), function(method, data, verbose, ...) {
  e = callNextMethod()
  valueColumn = formula(method) %>% getResponse

  # Model specification
  cat(verbose, 'Creating clusterLongData object...', level=verboseLevels$finest)

  parRefArgs = list(saveFreq = 1e99, scale=FALSE)
  parArgs = modifyList(parRefArgs, as.list(method), keep.null=TRUE)
  parArgs[setdiff(names(parArgs), formalArgs(parALGO))] = NULL
  e$par = do.call(parALGO, parArgs)

  e$cld = clusterLongData(traj=e$dataMat, idAll=rownames(e$dataMat), time=sort(unique(data[[method$time]])))
  return(e)
})


setMethod('fit', signature('clMethodKML'), function(method, data, envir, verbose, ...) {
  e = new.env(parent=envir)

  cld = envir$cld
  # Helper variables
  valueColumn = formula(method) %>% getResponse
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
