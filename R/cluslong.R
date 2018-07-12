#' @export
#' @import data.table
#' @import assertthat
#' @import magrittr
#' @importFrom R.utils printf
#' @title Cluster a longitudinal dataset
#' @seealso \link{cluslong_kml}, \link{cluslong_gckm}, \link{cluslong_gbtm}, \link{cluslong_gmm}, \link{cluslong_mixtvem}, \link{cluslong_twostep}
#' @param data Longitudinal \code{data.frame}, \code{data.table}, or a \link[=cluslongRecord]{CluslongRecord} object.
#' @param method The longitudinal cluster method to apply (kml, gckm, gbtm, gmm, mixtvem)
#' @param idCol Name of the column indicating the trajectory to which the observation belongs. Ignored when \code{data} is a CluslongRecord.
#' @param timeCol Name of the column containing the observation times. Ignored when \code{data} is a CluslongRecord.
#' @param valueCol Name of the column containing the observed values. Ignored when \code{data} is a CluslongRecord.
#' @param numClus The series of number of clusters to analyze.
#' @param numRuns Number of runs of the method. The effect is dependent on the method; being either the number of repeated starts or complete evaluations.
#' @param maxIter The maximum number of iterations in case convergence is not reached.
#' @param ... Method-specific arguments
#' @param result Function for computing additional results on each CluslongResult object (e.g. criteria)
#' @param keep The level of model output to preserve (all, minimal, none). This reduces the size of the output.
#' @param verbose Whether to enable verbose console output
#' @param seed Set the seed for RNG
#' @param catchError Whether to catch errors during the clustering, outputting an empty result. Prints the error as a warning.
#' @return If the \code{data} parameter was a \code{data.frame} or \code{data.table}, a new \code{CluslongRecord} object is returned. Otherwise, nothing is returned, but the \code{data} variable is updated with the results.
cluslong = function(data,
                    method,
                    numClus=2:5,
                    numRuns,
                    maxIter,
                    ...,
                    idCol=NULL,
                    timeCol=NULL,
                    valueCol=NULL,
                    verbose,
                    seed=NULL,
                    result=getOption('cluslong.result'),
                    keep=getOption('cluslong.keep', 'all'),
                    catchError=getOption('cluslong.catchError', FALSE)
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
                                prepFun=NULL,
                                clusterFun,
                                ...,
                                idCol,
                                timeCol,
                                valueCol,
                                result=NULL,
                                keep,
                                verbose,
                                seed,
                                catchError) {
    assert_that(is.numeric(numClus), noNA(numClus), all(numClus >= 1), all(numClus %% 1 == 0))
    assert_that(is.count(maxIter+1), maxIter >= 0)
    assert_that(is.null(result) || is.function(result))
    assert_that(is.scalar(keep), keep %in% c('all', 'minimal', 'none'))
    assert_that(is.flag(verbose))
    assert_that(is.flag(catchError))

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

    ## Preparation
    startTime = Sys.time()

    if(is.null(prepFun)) {
        prepVars = list()
    } else {
        userArgs = as.list(match.call()[-1])
        expectedArgNames = formalArgs(prepFun)
        prepVars = do.call(prepFun, subset_list(c(clr=clr, userArgs), expectedArgNames))
    }

    clusArgNames = formalArgs(clusterFun) %>% setdiff(c('clr', 'prepVars', 'startTime', 'nc'))
    clusArgs = as.list(match.call())[clusArgNames]

    assert_that(all(clusArgNames %in% names(clusArgs)),
                msg=paste0('missing argument(s): ', paste(setdiff(clusArgNames, names(clusArgs)), collapse=', ')))

    ## Clustering
    for(nc in numClus) {
        if(verbose) {
            printf('- Analyzing for clusters = %d\n', nc)
        }
        set.seed(seed)
        clResult = condTryCatch(
            cond=catchError,
            expr=do.call(clusterFun, c(list(clr=clr, prepVars=prepVars, nc=nc, startTime=startTime), clusArgs)),
            error=function(e) {
                warning(sprintf('Error occurred while clustering with numClus=%d: "%s"', nc, e$message), immmediate.=TRUE)
                cluslongResult(clr, nc,
                               clusters=rep('A', length(getIds(clr))),
                               trends=data.table(Cluster='A', Time=getTimes(clr), Value=0) %>%
                                   setnames(c('Time', 'Value'), c(clr@timeCol, clr@valueCol)),
                               start=startTime,
                               runTime=NA+0,
                               criteria=numeric(0),
                               converged=NA,
                               model=NULL)
            }
        )

        if(!is.null(result)) {
            if(verbose) {
                printf('- Running result function...\n')
            }
            clResult = condTryCatch(
                cond=catchError,
                expr=result(clr, clResult),
                error=function(e) {
                    warning(sprintf('Error occurred while evaluating result() for numClus=%d: "%s"', nc, e$message), immmediate.=TRUE)
                    clResult
                }
            )
            assert_that(is.CluslongResult(clResult), msg='result() function returned an object that is not a CluslongResult')
        }

        clr = update_clr(clr, clResult, clrName, clrEnv, verbose)
    }

    if(verbose) {
        printf('  Total time: %g seconds\nCompleted.', round(as.numeric(Sys.time() - startTime), 2))
    }

    if(is.null(clrName)) {
        return(clr)
    }
}

update_clr = function(clr, clResult, clrName, envir, verbose) {
    assert_that(is.CluslongResult(clResult))
    clr@results[[paste0('c', clResult@numClus)]] = clResult

    # ensure order
    ncs = sapply(clr@results, 'slot', 'numClus')
    if(is.unsorted(ncs)) {
        clr@results = clr@results[order(ncs)]
    }

    if(!is.null(clrName)) {
        assign(clrName, clr, envir=envir)
        if(verbose) {
            printf('  Updated cluslong record with results (in var "%s")\n', as.character(clrName))
        }
    }
    return(clr)
}


create_result_update_function = function(clrName, envir, result, verbose) {
    return(function(clr, clResults) {
        assert_that(is.CluslongRecord(clr))
        assert_that(is.list(clResults))
        assert_that(all(sapply(clResults, 'is', 'CluslongResult')))

        names(clResults) = paste0('c', sapply(clResults, slot, 'numClus'))
        clResults = lapply(clResults, function(clResult) result(clr, clResult))
        assert_that(all(sapply(clResults, 'is', 'CluslongResult')), msg='the supplied result function did not output a CluslongResult object')
        clr@results = modifyList(clr@results, clResults)

        if(!is.null(clrName)) {
            assign(clrName, clr, envir=envir)
            if(verbose) {
                printf('  Updated cluslong record with results (in var "%s")\n', as.character(clrName))
            }
        }
        return(clr)
    })
}