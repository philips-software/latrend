#' @export
#' @import lcmm
#' @title Growth mixture modeling
#' @inheritParams cluslong
#' @param fixed Fixed effects
#' @param random Random effects
#' @param mixture Class-specific effects
#' @param diagCov Whether to use a diagonal variance-covariance matrix
#' @param classCov Whether to use a class-specific variance-covariance matrix
#' @param start Model initialization method. Either gridsearch or gckm
#' @param startMaxIter Number of model optimization iterations for the initialization method
cluslong_gmm = function(data,
                        numClus=2:3,
                        maxIter=NULL,
                        numRuns=25,
                        fixed,
                        random,
                        mixture,
                        diagCov=TRUE,
                        classCov=FALSE,
                        start=c('gridsearch', 'gckm'),
                        startMaxIter=ifelse(start[1] == 'gridsearch', 10, 0),
                        idCol,
                        timeCol,
                        valueCol,
                        resultFun=function(clr, cluslongResult) cluslongResult,
                        keep=getOption('cluslong.keep', 'all'),
                        verbose=TRUE,
                        seed=NULL) {
    do.call(cluster_longitudinal, c(clusterFun=cl_gmm, mget(names(formals()), sys.frame(sys.nframe()))))
}



cl_gmm = function(clr, updateFun, numClus, numRuns, maxIter, fixed, random, mixture, diagCov, classCov, start, startMaxIter, keep, verbose) {
    valueCol = clr@valueCol

    assert_that(is.character(start))
    assert_that(is.flag(diagCov), is.flag(classCov), is.flag(verbose))

    if(verbose) {
        message('-- GMM analysis --')
    }

    tStart = Sys.time()

    for(g in numClus) {
        if(verbose) {
            message(sprintf('For nclus=%d --', g))
        }

        ## Model initialization
        gmmArgs = list(fixed=fixed, random=random, mixture=mixture, subject=clr@idCol,
                                  ng=g, idiag=diagCov, nwg=classCov, data=clr@data)
        initGmm = switch(start[1], gridsearch=initGmm_gridsearch, gckm=initGmm_gckm, stop('unknown start method'))

        if(verbose) {
            message(sprintf('Initializing model using %s approach...', start[1]))
        }
        tInit = Sys.time()
        modelArgs = initGmm(gmmArgs, numRuns=numRuns, maxIter=maxIter, verbose=verbose)
        modelArgs$maxiter = maxIter
        modelArgs$verbose = verbose
        initTime = as.numeric(Sys.time() - tInit)

        if(verbose) {
            message(sprintf('\tTook %g seconds', round(initTime, 2)))
        }

        ## Final model optimization
        if(verbose) {
            message('Optimizing final model...')
        }
        tRun = Sys.time()
        model = do.call('hlme', modelArgs)
        runTime = as.numeric(Sys.time() - tRun)

        ## Results
        if(verbose) {
            message('Computing results...')
        }
        clResult = gmm_result(clr, model, keep=keep, start=tStart, runTime=runTime, initTime=initTime)
        clr = updateFun(clr, list(clResult))
    }

    return(clr)
}



initGmm_gridsearch = function(gmmArgs, numRuns, maxIter, verbose) {
    gmmArgs$maxiter = maxIter
    gmmArgs$verbose = verbose

    gcmArgs = gmmArgs
    gcmArgs$ng = 1
    gcmArgs$mixture = NULL
    gcmArgs$verbose = verbose
    gcm = do.call('hlme', gcmArgs)

    e = environment()
    models = lapply(1:numRuns, function(i) {
        gmmArgs$B = substitute(random(gcm), env=e)
        if(verbose) {
            cat(sprintf('Run %d/%d: ', i, numRuns))
        }
        do.call('hlme', gmmArgs)
    })

    logLiks = sapply(models, '[[', 'loglik')
    bestModel = models[[which.max(logLiks)]]
    gmmArgs$B = bestModel$best
    return(gmmArgs)
}



initGmm_gckm = function(gmmArgs, numRuns, maxIter, verbose) {

}



gmm_result = function(clr, model, keep, start, runTime, initTime) {
    idCol = clr@idCol
    timeCol = clr@timeCol
    valueCol = clr@valueCol
    numClus = model$ng
    clusNames = LETTERS[1:numClus]
    clusters = factor(model$pprob$class, levels=1:numClus, labels=clusNames)
    assert_that(noNA(clusters))

    postProbs = as.matrix(model$pprob[, -1:-2])
    assert_that(nrow(postProbs) == length(getIds(clr)), ncol(postProbs) == numClus)
    colnames(postProbs) = clusNames
    rownames(postProbs) = getIds(clr)

    xmodel = switch(keep,
                    all=model,
                    minimal={m = model; m[c('pred', 'call', 'dataset', 'pprob', 'predRE')] = NULL; m},
                    none=NULL)

    # Compute trends
    assert_that(all(model$pred$Id == as.numeric(clr@data[[idCol]])))
    dt_trajmarg = data.table(model$pred[grep('Id|pred_m\\d', names(model$pred))], Time=clr@data[[timeCol]]) %>%
        melt(id=c(idCol, timeCol), variable.name='Cluster', value.name=valueCol)
    dt_trends = unique(dt_trajmarg[, -idCol, with=FALSE], by=c(timeCol, 'Cluster')) %>%
        .[, Cluster := factor(as.integer(Cluster), levels=1:numClus, labels=clusNames)] %>%
        setcolorder(c('Cluster', timeCol, valueCol))

    criteria = numeric()
    criteria['BIC'] = model$BIC
    criteria['AIC'] = model$AIC
    criteria['logLik'] = model$loglik

    details = list()
    details$initTime = initTime
    details$nIter = model$niter

    clResult = cluslongResult(clr, clusters=clusters,
                              trends=dt_trends,
                              start=start,
                              runTime=runTime,
                              criteria=criteria,
                              converged=model$conv == 1,
                              model=xmodel,
                              details=details)

    return(clResult)
}