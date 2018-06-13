#' @export
#' @import splines
#' @title Mixed time-varying effects modeling
#' @inheritParams cluslong
#' @param numKnots Number of interior knots for the B-spline representation
#' @param degree Degree of the polynomial between knots
#' @param assumeIndependence Whether to assume measurements are independent, or to model dependency using an AR(1) model
#' @param maxVarianceRatio Maximum ratio between class variances
#' @param scovCol Column name(s) of covariate(s) used for subject class membership
#' @param xcovCol Column name(s) of time-invariant covariate(s)
#' @param start Model initialization method. Either gridsearch or gckm
#' @param startMaxIter Number of model optimization iterations for the initialization method
cluslong_mixtvem = function(data,
                        numClus=2:3,
                        maxIter=1000,
                        numRuns=20,
                        numKnots=6,
                        degree=3,
                        assumeIndependence=TRUE,
                        maxVarianceRatio=10,
                        scovCol=NULL,
                        xcovCol=NULL,
                        idCol,
                        timeCol,
                        valueCol,
                        resultFun=function(clr, cluslongResult) cluslongResult,
                        keep=getOption('cluslong.keep', 'all'),
                        verbose=TRUE,
                        seed=NULL) {
    do.call(cluster_longitudinal, c(clusterFun=cl_mixtvem, mget(names(formals()), sys.frame(sys.nframe()))))
}


cl_mixtvem = function(clr, updateFun, numClus, numRuns, maxIter, numKnots, degree, assumeIndependence, maxVarianceRatio, scovCol, xcovCol, keep, verbose) {
    assert_that(is.scalar(maxIter), is.numeric(maxIter), maxIter >= 0)
    assert_that(is.scalar(numKnots), is.numeric(numKnots), numKnots >= 1)
    assert_that(is.scalar(degree), is.numeric(degree), degree >= 1)
    assert_that(is.scalar(maxVarianceRatio), is.numeric(maxVarianceRatio))
    assert_that(is.flag(assumeIndependence), is.flag(assumeIndependence))

    idCol = clr@idCol
    timeCol = clr@timeCol
    valueCol = clr@valueCol

    if(verbose) {
        message('-- MixTVEM analysis --')
    }

    xdata = copy(clr@data)
    xdata[, MixTvemIntercept := 1]

    tStart = Sys.time()
    for(g in numClus) {
        if(verbose) {
            message(sprintf('For nclus=%d --', g))
        }

        tRun = Sys.time()
        model = tryCatch({
            models = TVEMMixNormal(
                dep=xdata[[valueCol]],
                id=xdata[[idCol]],
                time=xdata[[timeCol]],
                tcov=xdata$MixTvemIntercept,
                maxIterations=maxIter,
                numClasses=g,
                numStarts=numRuns,
                numInteriorKnots=numKnots,
                deg=degree,
                assumeIndependence=assumeIndependence,
                maxVarianceRatio=maxVarianceRatio,
                getSEs=FALSE,
                doPlot=FALSE)
            models$bestFit
        }, error=function(e) {
            print(e)
            #TODO
            model = list(converged=FALSE, bic=NA, aic=NA, logLik=NA, weightedRSS=NA, weightedGCV=NA,
                         fittedY=matrix(0, nrow=nrow(clr@data), ncol=g))
        })
        runTime = as.numeric(Sys.time() - tRun)

        ## Results
        if(verbose) {
            message('Computing results...')
        }

        clResult = mixtvem_result(clr, numClus=g, model=model, keep=keep, start=tStart, runTime=runTime)

        clr = updateFun(clr, list(clResult))
    }

    return(clr)
}

mixtvem_result = function(clr, numClus, model, keep, start, runTime) {
    clusNames = LETTERS[1:numClus]
    clusters = factor(apply(model$postProbsBySub, 1, which.max), levels=1:numClus, labels=clusNames)

    postProbs = model$postProbsBySub
    assert_that(nrow(postProbs) == length(getIds(clr)), ncol(postProbs) == numClus)
    colnames(postProbs) = clusNames
    rownames(postProbs) = getIds(clr)

    xmodel = switch(keep, all=model,
                          minimal={modc = model; modc[c('residsY', 'postProbsByAssessment', 'fittedY', 'intId', 'dep', 'postProbsBySub', 'fittedProb')] = NULL; modc},
                          none=NULL)

    # Compute trends
    assert_that(nrow(model$fittedY) == nrow(clr@data))
    dt_trajmarg = data.table(model$fittedY, Id=clr@data[[clr@idCol]], Time=clr@data[[clr@timeCol]]) %>%
        melt(id=c('Id', 'Time'), variable.name='Cluster', value.name='Value')
    dt_trends = unique(dt_trajmarg[, -'Id'], by=c('Time', 'Cluster')) %>%
        .[, Cluster := factor(as.integer(Cluster), levels=1:numClus, labels=clusNames)] %>%
        setcolorder(c('Cluster', 'Time', 'Value')) %>%
        setnames(c('Time', 'Value'), c(clr@timeCol, clr@valueCol))

    # Criteria
    criteria = numeric()
    criteria['BIC'] = model$bic
    criteria['AIC'] = model$aic
    criteria['logLik'] = model$logLik
    criteria['WRSS'] = model$weightedRSS
    criteria['WGCV'] = model$weightedGCV

    details = list()
    details$proportionNugget = model$proportionNugget
    details$nIter = model$iteration

    clResult = cluslongResult(clr, clusters=clusters,
                              trends=dt_trends,
                              start=start,
                              runTime=runTime,
                              criteria=criteria,
                              converged=model$converged,
                              model=xmodel,
                              details=details)
    return(clResult)
}