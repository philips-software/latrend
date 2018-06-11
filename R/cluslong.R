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
                    resultFun=function(clr, cluslongResult) cluslongResult,
                    keep=getOption('cluslong.keep', 'all'),
                    verbose=TRUE,
                    seed=NULL
                    ) {

    if(is.CluslongRecord(data)) {
        clr = data
        if(sys.nframe() > 2) {
            # check if this is a call from another cluslong method
            caller = as.character(sys.calls()[[sys.nframe() - 2]][[1]])
            if(caller %in% ls('package:cluslong', pattern='cluslong_')) {
                parentEnv = parent.frame(2)
                clrName = deparse(substitute(data, env=parent.frame()))
            } else {
                parentEnv = parent.frame()
                clrName = deparse(substitute(data))
            }
        } else {
            parentEnv = parent.frame()
            clrName = deparse(substitute(data))
        }

        if(!exists(clrName, envir=parentEnv)) {
            clrName = NULL
        }
    } else {
        clr = cluslongRecord(data, idCol, timeCol, valueCol)
        clrName = NULL
    }

    assert_that(is.character(method))
    assert_that(uniqueN(getIds(clr)) > 1, msg='data contains only 1 time series')
    assert_that(uniqueN(getTimes(clr)) > 1, msg='data is not longitudinal; contains only 1 assessment time')
    assert_that(is.numeric(clr@data[[clr@valueCol]]), msg='longitudinal values are not numeric')
    assert_that(!any(is.infinite(clr@data[[clr@valueCol]])), msg='data contains infinite values')

    funpair = switch(method,
                     kml=c(cluslong_kml, cl_kml),
                     gckm=c(cluslong_gckm, cl_gckm),
                     stop('unknown method'))

    updateFun = create_result_update_function(clrName=clrName, envir=parentEnv, resultFun=resultFun, verbose=verbose)

    expectedArgNames = formalArgs(funpair[[2]])
    # get default arguments of the method
    defArgs = formals(funpair[[1]])[expectedArgNames] %>% as.list
    # overwrite with defaults of this function
    args = modifyList(defArgs, formals()[expectedArgNames])
    # overwrite with user's input
    finalArgs = c(clr=clr, updateFun=updateFun, modifyList(args, as.list(match.call())[expectedArgNames]))
    assert_that(all(expectedArgNames %in% names(finalArgs)),
                msg=paste0('missing argument(s): ', paste(setdiff(expectedArgNames, names(finalArgs)), collapse=', ')))

    cluslongCall = quote(do.call(funpair[[2]], finalArgs))

    ## Preparation
    if(verbose) {
        message(sprintf('Setting RNG seed to %d', seed))
    }
    set.seed(seed)

    ## Computation
    clr = eval(cluslongCall)

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