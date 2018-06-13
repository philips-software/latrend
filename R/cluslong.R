#' @export
#' @import data.table
#' @import assertthat
#' @import magrittr
#' @title Cluster a longitudinal dataset
#' @param data longitudinal data.frame or data.table, or a CluslongRecord object
#' @param method The longitudinal method to apply (kml, gckm, gbtm, gmm, mixtvem)
#' @param idCol Id column name for the longitudinal data
#' @param timeCol Time column name for the longitudinal data
#' @param valueCol Value column name for the longitudinal data
#' @param numClus Number of clusters; multiple values can be specified
#' @param numRuns Number of runs for methods that use repeated starts or evaluations
#' @param maxIter Maximum number of iterations in case convergence is not reached
#' @param ... Method-specific arguments
#' @param resultFun Function for computing additional results on each CluslongResult object (e.g. criteria)
#' @param keep The level of model output to preserve (all, minimal, none)
#' @param verbose Whether to enable verbose console output
#' @param seed Set the seed for RNG
#' @return If data was a data.frame/table, a new CluslongRecord object is returned. Otherwise, nothing is returned, but the original data input variable is updated.
cluslong = function(data,
                    method,
                    numClus=2:5,
                    numRuns,
                    maxIter,
                    ...,
                    idCol,
                    timeCol,
                    valueCol,
                    resultFun,
                    keep,
                    verbose,
                    seed=NULL
                    ) {

    clusterMethod = ls('package:cluslong', pattern=paste0('cluslong_', method))[1]

    defaultArgs = formals() %>% as.list
    userArgs = as.list(match.call()[-1])
    allArgs = modifyList(defaultArgs, userArgs)
    argNames = setdiff(names(allArgs), c('method', '...'))
    do.call(clusterMethod, allArgs[argNames], envir=parent.frame())
}

cluster_longitudinal = function(data,
                                numClus,
                                numRuns,
                                maxIter,
                                clusterFun,
                                ...,
                                idCol,
                                timeCol,
                                valueCol,
                                resultFun,
                                keep,
                                verbose,
                                seed) {
    assert_that(is.numeric(numClus))
    assert_that(is.null(maxIter) || (is.scalar(maxIter) && is.numeric(maxIter) && maxIter >= 0))
    assert_that(is.function(resultFun))
    assert_that(is.scalar(keep), keep %in% c('all', 'minimal', 'none'))
    assert_that(is.flag(verbose))

    if(is.CluslongRecord(data)) {
        clr = data
        clrEnv = parent.frame(2)
        clrName = deparse(substitute(data, env=parent.frame()))

        if(!exists(clrName, envir=clrEnv)) {
            warning(sprintf('unable to find target clusterLongRecord variable "%s" in the parent environment. Result will be returned instead.', clrName))
            clrName = NULL
        }
    } else {
        clr = cluslongRecord(data, idCol, timeCol, valueCol)
        clrName = NULL
    }

    assert_that(uniqueN(getIds(clr)) > 1, msg='data contains only 1 time series')
    assert_that(uniqueN(getTimes(clr)) > 1, msg='data is not longitudinal; contains only 1 assessment time')
    assert_that(is.numeric(clr@data[[clr@valueCol]]), msg='longitudinal values are not numeric')
    assert_that(!any(is.infinite(clr@data[[clr@valueCol]])), msg='data contains infinite values')

    updateFun = create_result_update_function(clrName=clrName, envir=clrEnv, resultFun=resultFun, verbose=verbose)

    clusArgNames = formalArgs(clusterFun)
    clusArgs = c(clr=clr, updateFun=updateFun, as.list(match.call()))[clusArgNames]

    assert_that(all(clusArgNames %in% names(clusArgs)),
                msg=paste0('missing argument(s): ', paste(setdiff(clusArgNames, names(clusArgs)), collapse=', ')))

    ## Preparation
    if(verbose) {
        message(sprintf('Setting RNG seed to %d', seed))
    }
    set.seed(seed)

    ## Computation
    clr = do.call(clusterFun, clusArgs)

    if(is.null(clrName)) {
        return(clr)
    }
}




create_result_update_function = function(clrName, envir, resultFun, verbose) {
    return(function(clr, clResults) {
        assert_that(is.CluslongRecord(clr))
        assert_that(is.list(clResults))
        assert_that(all(sapply(clResults, 'is', 'CluslongResult')))

        names(clResults) = paste0('c', sapply(clResults, slot, 'numClus'))
        clResults = lapply(clResults, function(clResult) resultFun(clr, clResult))
        assert_that(all(sapply(clResults, 'is', 'CluslongResult')), msg='the supplied resultFun function did not output a CluslongResult object')
        clr@results = modifyList(clr@results, clResults)

        if(!is.null(clrName)) {
            assign(clrName, clr, envir=envir)
            if(verbose) {
                message(sprintf('Updated cluslong record with results (in var "%s")', as.character(clrName)))
            }
        }
        return(clr)
    })
}