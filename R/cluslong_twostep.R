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
                            resultFun=NULL,
                            keep=getOption('cluslong.keep', 'all'),
                            verbose=TRUE,
                            seed=NULL,
                            catchError=FALSE) {

    assert_that(is.function(representStep), is.function(clusterStep), is.function(trendEstimator))
    assert_that(is.function(standardize) || isFALSE(standardize))

    if(verbose) {
        printf('=== Two-step analysis ===\n')
    }

    do.call(cluster_longitudinal, c(prepFun=prep_twostep, clusterFun=cl_twostep, mget(names(formals()), sys.frame(sys.nframe()))))
}

prep_twostep = function(clr, representStep, standardize, verbose) {
    if(verbose) {
        printf('- [1.] Representation step\n')
    }
    step1out = representStep(clr)
    ids = getIds(clr)

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

    if(verbose) {
        printf('- [2.] Cluster step\n')
    }

    return(list(idCoefs=idCoefs, idPreds=idPreds, X=X))
}

cluster_twostep = function(clr, prepVars, nc, startTime, numRuns, maxIter,
                      representStep, standardize, clusterStep, trendEstimator,
                      verbose) {
    valueCol = clr@valueCol

    tRunStart = Sys.time()
    step2out = clusterStep(prepVars$X, nc)
    runTime = as.numeric(Sys.time() - tRunStart)

    if(verbose) {
        printf('  Took %g seconds.\n', round(runTime, 2))
        printf('- Computing results...\n')
    }
    results = list(clusters=NULL, criteria=c(), converged=NA, model=NULL)
    if(is.list(step2out)) {
        results = modifyList(results, step2out)
    } else {
        assert_that(is.numeric(step2out) || is.factor(step2out), msg='expecting cluster assignment output from the cluster step (numeric or factor)')
        results$clusters = step2out
    }

    assert_that(noNA(results$clusters))
    assert_that(length(results$clusters) == length(getIds(clr)))

    # Compute trends
    trends = trendEstimator(clr, clusters=results$clusters, coefs=prepVars$idCoefs, preds=prepVars$idPreds, model=results$model)

    do.call(cluslongResult, c(list(clr=clr, numClus=nc, trends=trends, start=startTime, runTime=runTime), results))
}