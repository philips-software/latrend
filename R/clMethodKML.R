#' @include clMethod.R
setClass('clMethodKML', contains='clMatrixMethod')

#' @export
#' @import kml
#' @import longitudinalData
#' @title Specify KML method
#' @param formula Formula. Only intercept is supported.
#' @param time Time variable.
#' @param id Strata variable.
#' @param nClusters Number of clusters.
#' @param nRuns Number of repeated runs among which the best fit is selected.
#' @param maxIter Maximum number of iterations without convergence.
#' @param start Internal initialization strategy.
#' @param imputation Name of the imputation method, see \link[longitudinalData]{imputation}. The default is to not impute the data, in which case an error is thrown when the data contains missing values.
#' @param distance Distance function or function name.
#' @param center Function specifying the computation of the cluster center.
#' @examples
#' method = clMethodKML(Measurement ~ 1,
#'                      time='Assessment',
#'                      id='Id', nClusters=3)
#' @family clMethod classes
clMethodKML = function(formula=Value ~ 1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       nRuns=20,
                       maxIter=200,
                       start='kmeans++',
                       imputation=NULL,
                       distance='euclidean',
                       center=meanNA) {
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

  if(isArgDefined(object, 'nRuns')) {
    assert_that(is.count(object$nRuns))
  }
})


setMethod('getName', signature('clMethodKML'), function(object) 'longitudinal k-means (KML)')

setMethod('getName0', signature('clMethodKML'), function(object) 'kml')


setMethod('prepare', signature('clMethodKML'), function(method, data, verbose, ...) {
  e = callNextMethod()
  valueColumn = formula(method) %>% getResponse

  # Check data
  if(is.null(method$imputation)) {
    assert_that(!anyNA(data[[valueColumn]]), msg='data contains missing values, with no imputation method specified')
  }

  # Model specification
  cat(verbose, 'Creating clusterLongData object...', verboseLevels$finest)
  e$par = parALGO(saveFreq = 1e99,
                scale = FALSE,
                maxIt = method$maxIter,
                startingCond = method$start,
                imputationMethod = ifelse(is.null(method$imputation), 'copyMean', method$imputation),
                distanceName = ifelse(is.character(method$distance), method$distance, ''),
                distance = method$distance,
                centerMethod = method$center)
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
  startTime = Sys.time()
  suppressFun(
    kml(cld, nbClusters=method$nClusters, nbRedrawing=method$nRuns, toPlot='none', parAlgo=envir$par)
  )
  runTime = as.numeric(Sys.time() - startTime)
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
