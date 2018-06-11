#' @export
#' @import kml
#' @import longitudinalData
#' @title Longitudinal k-means
#' @param data Longitudinal data.frame or data.table
#' @param id Id column name
#' @param time Time column name
#' @param numClus Number of clusters
#' @param numRuns Number of runs
#' @param maxIter Maximum number of iterations in case convergence is not reached
#' @param start Method for initializing the model
#' @param distance Name of the distance function, or a function of d(A, B)
#' @param center Function specifying the computation of the cluster center
#' @param keep
#' @param verbose
cluslong_kml = function(data,
                        idCol,
                        timeCol,
                        valueCol,
                        numClus=1:6,
                        numRuns=10,
                        maxIter=200,
                        start='kmeans++',
                        imputation='copyMean',
                        distance='euclidean',
                        center=meanNA,
                        keep=getOption('cluslong.keep', 'all'),
                        verbose=TRUE) {
    do.call(cluslong, c(method='kml', mget(names(formals()), sys.frame(sys.nframe()))))
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
    wideData = dcast(clr@data, get(clr@idCol) ~ get(clr@timeCol), value.var='Value')
    datamat = as.matrix(wideData[,-'clr'])
    rownames(datamat) = wideData$clr

    ## Method
    par = parALGO(saveFreq=1e99, scale=FALSE, maxIt=maxIter, startingCond=start, imputationMethod=imputation, distanceName=distance, centerMethod=center)
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
    xmodel = kml_model(model, keep)

    rowAssignments = rep(clusters, clr@data[, .N, by=c(clr@idCol)]$N)
    rowTimes = clr@data[[clr@timeCol]]
    dt_trends = clr@data[, .(Value=center(get(valueCol))), keyby=.(Cluster=rowAssignments, Time=rowTimes)]
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
                              converged=TRUE)

    return(clResult)
}


kml_model = function(model, keep) {

}