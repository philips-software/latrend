#' @export
#' @title Generic two-step approach to clustering longitudinally
#' @inheritParams cluslong
#' @param representStep Function that computes the time series representation
#' @param standardize Scaling function to standardize the coefficients prior to clustering
#' @param clusterStep Function that clusters the coefficients
#' @param trendEstimator Function that computes the group trend given the cluster assignments
cluslong_twostep = function(data,
                            numClus=1:4,
                            numRuns=10,
                            maxIter=200,
                            representStep,
                            standardize=scale,
                            clusterStep,
                            trendEstimator,
                            idCol,
                            timeCol,
                            valueCol,
                            resultFun=function(clr, cluslongResult) cluslongResult,
                            keep=getOption('cluslong.keep', 'all'),
                            verbose=TRUE,
                            seed=NULL) {
    do.call(cluster_longitudinal, c(clusterFun=cl_twostep, mget(names(formals()), sys.frame(sys.nframe()))))
}



cl_twostep = function(clr, updateFun, numClus, numRuns, maxIter,
                      representStep, standardize, clusterStep, trendEstimator,
                      verbose) {
    valueCol = clr@valueCol
    ids = getIds(clr)

    assert_that(is.function(representStep), is.function(clusterStep))
    assert_that(is.function(trendEstimator))
    assert_that(is.function(standardize) || isFALSE(standardize))

    if(verbose) {
        message('-- Two-step analysis --')
    }

    startTime = Sys.time()
    ## Representation step
    if(verbose) {
        message('(1) Representation step')
    }
    step1out = representStep(clr)
    if(is.list(step1out)) {
        assert_that(all(c('coefs', 'preds') %in% names(step1out)))
        idCoefs = as.matrix(step1out$coefs)
        idPreds = step1out$preds
    } else {
        idCoefs = step1out
        idPreds = NA[seq_along(ids)]
    }

    assert_that(is.matrix(idCoefs), nrow(idCoefs) == length(ids))
    assert_that(is.numeric(idCoefs), is.numeric(idPreds))
    assert_that(length(idPreds) == nrow(clr@data))
    rownames(idCoefs) = getIds(clr)

    ## Standardize
    if(is.function(standardize)) {
        X = standardize(idCoefs)
    } else {
        X = idCoefs
    }

    ## Cluster step
    if(verbose) {
        message('(2) Cluster step')
    }
    for(g in numClus) {
        if(verbose) {
            message(sprintf('For %d clusters...', g))
        }
        tRunStart = Sys.time()
        step2out = clusterStep(X, g)
        runTime = as.numeric(Sys.time() - tRunStart)

        if(verbose) {
            message(sprintf('\tTook %g seconds.', round(runTime, 2)))
            message('Computing results..', appendLF=FALSE)
        }
        results = list(clusters=NULL, criteria=c(), converged=NA, model=NULL)
        if(is.list(step2out)) {
            results = modifyList(results, step2out)
        } else {
            assert_that(is.numeric(step2out) || is.factor(step2out), msg='expecting cluster assignment output from the cluster step (numeric or factor)')
            results$clusters = step2out
        }

        assert_that(noNA(results$clusters))
        assert_that(length(results$clusters) == length(ids))

        # Compute trends
        trends = trendEstimator(clr, clusters=results$clusters, coefs=idCoefs, preds=idPreds, model=results$model)

        clResult = do.call(cluslongResult, c(list(clr=clr, trends=trends, start=startTime, runTime=runTime), results))

        if(verbose) {
            message('.')
        }
        clr = updateFun(clr, list(clResult))
    }


    return(clr)
}