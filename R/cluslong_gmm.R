#' @export
#' @import lcmm
#' @title Growth mixture modeling
#' @inheritParams cluslong
#' @param fixed Fixed effects.
#' @param random Random effects.
#' @param mixture Class-specific effects.
#' @param classmb Formula for the multinomial class membership model. Intercept should NOT be included.
#' @param diagCov Whether to use a diagonal variance-covariance matrix
#' @param classCov Whether to use a class-specific variance-covariance matrix
#' @param start Model initialization method name; gridsearch, gckm, or kml. Alternatively, a custom \code{function(clr, nc, gmmArgs, numRuns, maxIter, verbose)} can be provided, returning the arguments to the \code{lcmm::hlme} call.
#' @param startMaxIter Number of model optimization iterations for the initialization method (passed to the \code{start} function as \code{maxIter})
cluslong_gmm = function(data,
                        numClus=2:3,
                        maxIter=0,
                        numRuns=25,
                        fixed,
                        random,
                        mixture,
                        classmb=~-1,
                        diagCov=TRUE,
                        classCov=FALSE,
                        start='gridsearch',
                        startMaxIter=ifelse(start == 'gridsearch', 20, 0),
                        idCol=NULL,
                        timeCol=NULL,
                        valueCol=NULL,
                        verbose=TRUE,
                        seed=NULL,
                        result=getOption('cluslong.result'),
                        keep=getOption('cluslong.keep', 'all'),
                        catchError=getOption('cluslong.catchError', FALSE)) {
    do.call(cluster_longitudinal, c(prepFun=prep_gmm, clusterFun=cluster_gmm, mget(names(formals()), sys.frame(sys.nframe()))))
}



prep_gmm = function(clr, fixed, random, mixture, classmb, start, startMaxIter, maxIter, diagCov, classCov, verbose) {
    assert_that(is.formula(fixed), is.formula(random), is.formula(mixture), is.formula(classmb))
    assert_that(is.scalar(start), is.character(start) || is.function(start))
    assert_that(is.count(startMaxIter+1), startMaxIter >= 0)
    assert_that(is.flag(diagCov), is.flag(classCov))

    if(verbose) {
        if(random == ~-1) {
            printf('=== GBTM analysis ===\n')
        } else {
            printf('=== GMM analysis ===\n')
        }
    }
}

cluster_gmm = function(clr, prepVars, nc, startTime, numRuns, maxIter,
                       fixed, random, mixture, classmb,
                       diagCov, classCov, start, startMaxIter, keep, verbose) {
    valueCol = clr@valueCol

    if(classCov && nc == 1) {
        classCov = FALSE
    }

    if(maxIter == 0) {
        maxIter = NULL
    }

    ## Model initialization
    gmmArgs = list(fixed=fixed, random=random, mixture=mixture, classmb=classmb, subject=clr@idCol,
                              ng=nc, idiag=diagCov, nwg=classCov, data=clr@data)

    if(nc == 1) {
        gmmArgs[['classmb']] = NULL #hlme expects classmb to be missing for nc = 1
    }

    initGmm = switch(tolower(start),
                     gridsearch = initGmm_gridsearch,
                     gckm = initGmm_gckm,
                     kml = initGmm_kml,
                     stop('unknown start method'))

    if(verbose) {
        if(is.character(start)) {
            printf('- Initializing model using %s approach...\n', start)
        } else {
            printf('- Initializing model using custom approach...\n')
        }
    }
    tInit = Sys.time()
    modelArgs = initGmm(clr, nc, gmmArgs, numRuns=numRuns, maxIter=startMaxIter, verbose=verbose)
    modelArgs$maxiter = maxIter
    modelArgs$verbose = verbose
    initTime = as.numeric(Sys.time() - tInit)

    if(verbose) {
        printf('  Took %g seconds\n', round(initTime, 2))
    }

    ## Final model optimization
    if(verbose) {
        printf('- Optimizing final model...\n')
    }
    tRun = Sys.time()
    model = do.call('hlme', modelArgs)
    runTime = as.numeric(Sys.time() - tRun)

    ## Results
    if(verbose) {
        printf('- Computing results...\n')
    }
    gmm_result(clr, model, keep=keep, start=startTime, runTime=runTime, initTime=initTime)
}



initGmm_gridsearch = function(clr, nc, gmmArgs, numRuns, maxIter, verbose) {
    if(maxIter == 0) {
        gmmArgs$maxiter = NULL
    } else {
        gmmArgs$maxiter = maxIter
    }
    gmmArgs$verbose = verbose

    gcmArgs = gmmArgs
    gcmArgs$ng = 1
    gcmArgs$nwg = FALSE
    gcmArgs$classmb = NULL
    gcmArgs$mixture = NULL
    gcmArgs$verbose = verbose
    if(verbose) {
        cat('GCM fit: ')
    }
    gcm = do.call('hlme', gcmArgs)
    if(verbose) {
        cat('---\n')
    }

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



initGmm_gckm = function(clr, nc, gmmArgs, numRuns, maxIter, verbose) {
    clrGckm = cluslong_gckm(clr@data, idCol=clr@idCol, timeCol=clr@timeCol, valueCol=clr@valueCol,
                            numClus=nc, numRuns=numRuns, maxIter=maxIter,
                            gcmFixed=gmmArgs$fixed,
                            gcmRandom=gmmArgs$mixture,
                            keep='all',
                            verbose=FALSE)
    clResult = getResults(clrGckm, nc)
    gmmCoefs_from_clusters(clr, gmmArgs, clusters=clResult@clusters, priors=getClusterProps(clResult))
}



initGmm_kml = function(clr, nc, gmmArgs, numRuns, maxIter, verbose) {
    clrKml = cluslong_kml(clr@data, idCol=clr@idCol, timeCol=clr@timeCol, valueCol=clr@valueCol,
                            numClus=nc, numRuns=numRuns, maxIter=maxIter,
                            keep='all',
                            verbose=FALSE)
    clResult = getResults(clrKml, nc)
    gmmCoefs_from_clusters(clr, gmmArgs, clusters=clResult@clusters, priors=getClusterProps(clResult))
}



# Fit a GCM per cluster
gmmCoefs_from_clusters = function(clr, gmmArgs, clusters, priors) {
    assert_that(formula(delete.response(terms(gmmArgs$fixed))) == gmmArgs$mixture, msg='Not supported. fixed and mixture effects should be the same')
    rowClusters = rep(clusters, clr@data[, .N, by=c(clr@idCol)]$N)

    if(gmmArgs$classmb != (~-1)) {
        warning('classmb not supported for cluster initialization. Selected starting values may be suboptimal')
    }

    gcmList = lapply(levels(clusters), function(clus) {
        gcmArgs = gmmArgs
        gcmArgs$data = clr@data[rowClusters == clus]
        gcmArgs$ng = 1
        gcmArgs$classmb = NULL
        gcmArgs$nwg = FALSE
        gcmArgs$mixture = NULL
        gcmArgs$verbose = FALSE
        do.call('hlme', gcmArgs)
    })
    nc = gmmArgs$ng

    prefClusIndex = which.max(priors)

    # construct the B vector
    classMbs = log(priors / last(priors))
    names(classMbs) = paste0('intercept.MB-c', 1:nc)
    covariates = sapply(gcmList, function(m) fixef(m)[[2]])
    rownames(covariates) = names(coef(gcmList[[1]])[1:nrow(covariates)])

    if(gmmArgs$random == ~ -1) {
        varCovs = NULL
        propVarCovs = NULL
    } else {
        if(gmmArgs$nwg) {
            classVarCovs = do.call(cbind, lapply(gcmList, function(m) {
                capture.output(varCov <- as.matrix(VarCovRE(m))[,1])
                return(varCov)
            }))
            varCovs = classVarCovs[ , nc]
            propVarCovs = colMeans(classVarCovs[ , -nc] / varCovs)
            names(propVarCovs) = paste0('varprop-c', 1:(nc-1))
        } else {
            capture.output(varCovs <- as.matrix(VarCovRE(gcmList[[prefClusIndex]]))[,1])
            propVarCovs = NULL
        }
    }
    res = abs(coef(gcmList[[prefClusIndex]])['stderr'])
    names(res) = 'stderr'

    gmmArgs$B = c(head(classMbs, -1),
                  setNames(t(covariates), paste0(rep(rownames(covariates), each=nc), '-c', rep(1:nc, nc))),
                  varCovs, propVarCovs, res)
    return(gmmArgs)
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
    predCols = c(idCol, grep('pred_m\\d', names(model$pred), value=TRUE))
    dt_trajmarg = data.table(model$pred[predCols], Time=clr@data[[timeCol]]) %>%
        melt(id=c(idCol, 'Time'), variable.name='Cluster', value.name=valueCol)
    dt_trends = unique(dt_trajmarg[, -idCol, with=FALSE], by=c('Time', 'Cluster')) %>%
        .[, Cluster := factor(as.integer(Cluster), levels=1:numClus, labels=clusNames)] %>%
        setnames('Time', timeCol) %>%
        setcolorder(c('Cluster', timeCol, valueCol))

    criteria = numeric()
    criteria['BIC'] = model$BIC
    criteria['AIC'] = model$AIC
    criteria['logLik'] = model$loglik

    details = list()
    details$initTime = initTime
    details$nIter = model$niter

    clResult = cluslongResult(clr, numClus=numClus,
                              clusters=clusters,
                              trends=dt_trends,
                              start=start,
                              runTime=runTime,
                              criteria=criteria,
                              converged=model$conv == 1,
                              model=xmodel,
                              details=details)

    return(clResult)
}
