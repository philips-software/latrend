#' @export
#' @import kml
#' @import longitudinalData
#' @title Longitudinal k-means
#' @inheritParams cluslong
#' @param start Method for initializing the model
#' @param distance Name of the distance function, or a function of d(A, B)
#' @param center Function specifying the computation of the cluster center
cluslong_kml = function(data,
                        numClus=2:3,
                        numRuns=10,
                        maxIter=200,
                        start='kmeans++',
                        imputation='copyMean',
                        distance='euclidean',
                        center=meanNA,
                        idCol,
                        timeCol,
                        valueCol,
                        resultFun=NULL,
                        keep=getOption('cluslong.keep', 'all'),
                        verbose=TRUE,
                        seed=NULL) {
    args = mget(names(formals()), sys.frame(sys.nframe()))
    do.call(cluster_longitudinal, c(prepFun=prep_kml, clusterFun=cluster_kml, args))
}



prep_kml = function(clr, maxIter, start, imputation, distance, center, verbose) {
    if(verbose) {
        message('=== KML analysis ===')
        suppressFun = function(x) x
    }
    else {
        suppressFun = capture.output
    }

    ## Input checks
    if(is.null(imputation)) {
        assert_that(!anyNA(clr@data[[clr@valueCol]]), msg='data contains missing values, with no imputation method specified')
    }
    assert_that(uniqueN(clr@data[, .N, by=c(clr@idCol)]$N) == 1, msg='not all time series are of equal length')

    ## Transform data
    wideData = dcast(clr@data, get(clr@idCol) ~ get(clr@timeCol), value.var=clr@valueCol)
    datamat = as.matrix(wideData[,-'clr'])
    rownames(datamat) = wideData$clr

    ## Method
    par = parALGO(saveFreq=1e99, scale=FALSE, maxIt=maxIter, startingCond=start, imputationMethod=imputation, distanceName=distance, distance=distance, centerMethod=center)
    cld = clusterLongData(traj=datamat, idAll=rownames(datamat), time=getTimes(clr))

    return(list(cld=cld, par=par, suppressFun=suppressFun))
}



cluster_kml = function(clr, prepVars, nc, startTime, numRuns, maxIter, center, keep, verbose) {
    cld = prepVars$cld
    tRunStart = Sys.time()
    prepVars$suppressFun(
        kml(cld, nbClusters=nc, nbRedrawing=numRuns, toPlot='none', parAlgo=prepVars$par)
    )
    runTime = as.numeric(Sys.time() - tRunStart)

    if(verbose) {
        message('- Computing results...')
    }
    kml_result(clr, cld, nc=nc, keep=keep, startTime=startTime, runTime=runTime, center=center)
}



kml_result = function(clr, cld, nc, keep, startTime, runTime, center) {
    idCol = clr@idCol
    timeCol = clr@timeCol
    valueCol = clr@valueCol
    clusters = getClusters(cld, nc)

    model = slot(cld, paste0('c', nc))[[1]]
    xmodel = switch(keep, all=model, minimal=NULL, none=NULL)

    rowClusters = rep(clusters, clr@data[, .N, by=c(idCol)]$N)
    rowTimes = clr@data[[timeCol]]
    dt_trends = clr@data[, .(Value=center(get(valueCol))), keyby=.(Cluster=rowClusters, Time=rowTimes)] %>%
        setnames(c('Cluster', timeCol, valueCol))
    postProbs = model@postProba
    colnames(postProbs) = levels(clusters)

    criteria = model@criterionValues
    criteria['BIC'] = -criteria['BIC']
    criteria['AIC'] = -criteria['AIC']

    clResult = cluslongResult(clr, numClus=nc,
                              clusters=clusters,
                              trends=dt_trends,
                              start=startTime,
                              runTime=runTime,
                              criteria=criteria,
                              converged=TRUE,
                              model=xmodel)

    return(clResult)
}
