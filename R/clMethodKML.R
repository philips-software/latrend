setClass('clMethodKML', contains='clMethod')

setValidity('clMethodKML', function(object) {
  call = getCall(object)
  assert_that(hasSingleResponse(object$formula))
  assert_that(!hasCovariates(object$formula), msg='covariates are not supported')

  assert_that(all(formalArgs(clMethodKML) %in% names(call)), msg='clMethod object is missing required arguments')

  assert_that(is.wholeNumber(object$nClusters))

  assert_that(is.wholeNumber(object$nRuns))
})


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
  new('clMethodKML', call=match.call.defaults())
}


setMethod('getName', signature('clMethodKML'), function(object) 'longitudinal k-means (KML)')

setMethod('getName0', signature('clMethodKML'), function(object) 'kml')


##
kml_prepare = function(method, data, control) {
  e = new.env()

  valueColumn = formula(method) %>% getResponse

  # Check data
  if(is.null(method$imputation)) {
    assert_that(!anyNA(data[[valueColumn]]), msg='data contains missing values, with no imputation method specified')
  }
  assert_that(uniqueN(data[, .N, by=c(method$id)]$N) == 1, msg='not all time series are of equal length')

  # Data
  logfine('Reshaping data...')
  wideFrame = dcast(data, get(method$id) ~ get(method$time), value.var=valueColumn)
  wideData = as.matrix(wideFrame[, -'method']) %>%
    set_rownames(wideFrame$method)

  # Parameter processing
  if(is.character(method$distance)) {
    distanceName = method$distance
  } else {
    distanceName = ''
  }

  # Model specification
  logfiner('Creating clusterLongData object...')
  par = parALGO(saveFreq = 1e99,
                scale = FALSE,
                maxIt = method$maxIter,
                startingCond = method$start,
                imputationMethod = ifelse(is.null(method$imputation), 'copyMean', method$imputation),
                distanceName = distanceName,
                distance = method$distance,
                centerMethod = method$center)
  assign('par', par, envir=e)

  cld = clusterLongData(traj=wideData, idAll=rownames(wideData), time=sort(unique(data[[method$time]])))
  assign('cld', cld, envir=e)

  return(e)
}
setMethod('prepare', signature('clMethodKML'), kml_prepare)


##
kml_fit = function(method, data, control, prepEnv) {
  e = new.env(parent=prepEnv)

  cld = prepEnv$cld
  # Helper variables
  valueColumn = formula(method) %>% getResponse
  suppressFun = if (getLogger()$level <= loglevels['INFO']) force else capture.output

  startTime = Sys.time()
  suppressFun(
    kml(cld, nbClusters=method$nClusters, nbRedrawing=method$nRuns, toPlot='none', parAlgo=prepEnv$par)
  )
  runTime = as.numeric(Sys.time() - startTime)
  assign('cld', cld, envir=e)

  return(e)
}
setMethod('fit', signature('clMethodKML'), kml_fit)


##
kml_finalize = function(method, data, control, fitEnv) {
  model = new('clModelKML',
              method=method,
              model=fitEnv$cld,
              clusterNames=genClusNames(method$nClusters))
  return(model)
}
setMethod('finalize', signature('clMethodKML'), kml_finalize)
