#' @include clMatrixMethod.R
setClass('clMethodCrimCV', contains='clMatrixMethod')

#' @export
#' @import crimCV
#' @title Specify zero-inflated repeated-measures GBTM method
#' @inheritParams clMatrixMethod
#' @inheritParams crimCV::crimCV
#' @examples
#' method = clMethodCrimCV(Value ~ 1, nClusters=3)
#' model = cluslong(method, testLongData)
#'
#' library(crimCV)
#' data(TO1adj)
#' method = clMethodCrimCV(Offenses ~ 1, time='Offense', id='Subject')
#' model = cluslong(method, TO1adj)
#' @family clMethod classes
clMethodCrimCV = function(formula=Value ~ 1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       dpolyp=3,
                       dpolyl=3,
                       model='ZIPt',
                       rcv=FALSE,
                       init=20, ...) {
  new('clMethodCrimCV', call=match.call.defaults())
}


setMethod('getName', signature('clMethodCrimCV'), function(object) 'zero-inflated GBTM using crimcv')

setMethod('getName0', signature('clMethodCrimCV'), function(object) 'crimcv')

setMethod('prepare', signature('clMethodCrimCV'), function(method, data, verbose, ...) {
  times = sort(unique(data[[method$time]]))

  refTimes = seq(first(times), last(times), length.out=length(times))
  assert_that(isTRUE(all.equal(times, refTimes)), msg='Measurement times must be equally spaced, starting from 0 and ending at 1. This requirement is enforced because crimCV internally specifies the measurement times in this way.')

  callNextMethod()
})

#' @importFrom crimCV crimCV
setMethod('fit', signature('clMethodCrimCV'), function(method, data, envir, verbose, ...) {
  e = new.env(parent=envir)
  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  args = as.list(method)
  args$Dat = envir$dataMat
  args$ng = method$nClusters
  args[setdiff(names(args), formalArgs(crimCV))] = NULL

  suppressFun({
    e$model = do.call(crimCV, args)
  })
  e$model$data = envir$dataMat
  e$model$minTime = min(data[[method$time]])
  e$model$durTime = max(data[[method$time]]) - e$model$minTime
  return(e)
})


setMethod('finalize', signature('clMethodCrimCV'), function(method, data, envir, verbose, ...) {
  assert_that(has_name(envir$model, 'beta'), msg='invalid crimCV model returned from fit. The model either failed to initialize, converge, or its specification is unsupported.')

  model = new('clModelCrimCV',
              method=method,
              data=data,
              model=envir$model,
              clusterNames=make.clusterNames(method$nClusters))
  return(model)
})
