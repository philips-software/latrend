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
                        resultFun=function(clr, cluslongResult) cluslongResult,
                        keep=getOption('cluslong.keep', 'all'),
                        verbose=TRUE,
                        seed=NULL) {
    args = mget(names(formals()), sys.frame(sys.nframe()))
    do.call(cluster_longitudinal, c(clusterFun=cl_kml, args))
}

cl_kml = function(clr, updateFun, numClus, numRuns, maxIter, start, imputation, distance, center, keep, verbose) {
    valueCol = clr@valueCol

    if(verbose) {
        message('-- KML analysis --')
        suppress = function(x) x
    }
    else {
        suppress = capture.output
    }

    ## Input checks
    if(is.null(imputation)) {
        assert_that(!anyNA(clr@data[[clr@valueCol]]), msg='data contains missing values, with no imputation method specified')
    }
    assert_that(uniqueN(clr@data[, .N, by=c(clr@idCol)]$N) == 1, msg='not all time series are of equal length')

    ## Transform data
    wideData = dcast(clr@data, get(clr@idCol) ~ get(clr@timeCol), value.var=valueCol)
    datamat = as.matrix(wideData[,-'clr'])
    rownames(datamat) = wideData$clr

    ## Method
    par = parALGO(saveFreq=1e99, scale=FALSE, maxIt=maxIter, startingCond=start, imputationMethod=imputation, distanceName=distance, distance=distance, centerMethod=center)
    cld = clusterLongData(traj=datamat, idAll=rownames(datamat), time=getTimes(clr))

    startTime = Sys.time()
    for(g in numClus) {
        if(verbose) {
            cat(sprintf('nclus=%d:\t', g))
        }
        tRunStart = Sys.time()
        suppress(kml(cld, nbClusters=g, nbRedrawing=numRuns, toPlot='none', parAlgo=par))
        runTime = as.numeric(Sys.time() - tRunStart)

        if(verbose) {
            message('Computing results..', appendLF=FALSE)
        }
        clResult = kml_result(clr, cld, g=g, keep=keep, start=startTime, runTime=runTime, center=center)
        if(verbose) {
            message('.')
        }
        clr = updateFun(clr, list(clResult))
    }

    return(clr)
}


kml_result = function(clr, cld, g, keep, start, runTime, center) {
    valueCol = clr@valueCol
    clusters = getClusters(cld, g)

    model = slot(cld, paste0('c', g))[[1]]
    xmodel = switch(keep, all=model, minimal=NULL, none=NULL)

    rowClusters = rep(clusters, clr@data[, .N, by=c(clr@idCol)]$N)
    rowTimes = clr@data[[clr@timeCol]]
    dt_trends = clr@data[, .(Value=center(get(valueCol))), keyby=.(Cluster=rowClusters, Time=rowTimes)] %>%
        setnames(c('Cluster', clr@timeCol, clr@valueCol))
    postProbs = model@postProba
    colnames(postProbs) = levels(clusters)

    criteria = model@criterionValues
    criteria['BIC'] = -criteria['BIC']
    criteria['AIC'] = -criteria['AIC']

    clResult = cluslongResult(clr, clusters=clusters,
                              trends=dt_trends,
                              start=start,
                              runTime=runTime,
                              criteria=criteria,
                              converged=TRUE,
                              model=xmodel)

    return(clResult)
}
