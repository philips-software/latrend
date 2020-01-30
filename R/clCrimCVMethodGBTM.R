#' @include clMatrixMethod.R
setClass('clCrimCVMethodGBTM', contains='clMatrixMethod')

#' @export
#' @import crimCV
#' @title Specify zero-inflated repeated-measures GBTM method
#' @inheritParams clMatrixMethod
#' @inheritParams crimCV::crimCV
#' @examples
#' method = clCrimCVMethodGBTM(Value ~ 1, nClusters=3)
#' model = cluslong(method, testLongData)
#'
#' library(crimCV)
#' data(TO1adj)
#' method = clCrimCVMethodGBTM(Offenses ~ 1, time='Offense', id='Subject')
#' model = cluslong(method, TO1adj)
#' @family clMethod classes
clCrimCVMethodGBTM = function(formula=Value ~ 1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       dpolyp=3,
                       dpolyl=3,
                       model='ZIPt',
                       rcv=FALSE,
                       init=20, ...) {
  new('clCrimCVMethodGBTM', call=match.call.defaults())
}


setMethod('getName', signature('clCrimCVMethodGBTM'), function(object) 'zero-inflated GBTM')

setMethod('getName0', signature('clCrimCVMethodGBTM'), function(object) 'gbtmzi')

setMethod('prepare', signature('clCrimCVMethodGBTM'), function(method, data) {
  times = sort(unique(data[[method$time]]))

  refTimes = seq(first(times), last(times), length.out=length(times))
  assert_that(isTRUE(all.equal(times, refTimes)), msg='Measurement times must be equally spaced, starting from 0 and ending at 1. This requirement is enforced because crimCV internally specifies the measurement times in this way.')

  callNextMethod()
})

#' @importFrom crimCV crimCV
setMethod('fit', signature('clCrimCVMethodGBTM'), function(method, data, prepEnv) {
  e = new.env(parent=prepEnv)

  suppressFun = if(canShowModelOutput()) force else capture.output

  startTime = Sys.time()

  args = as.list(method)
  args$Dat = prepEnv$dataMat
  args$ng = method$nClusters
  args[setdiff(names(args), formalArgs(crimCV))] = NULL

  suppressFun({
    e$model = do.call(crimCV, args)
  })
  e$model$data = prepEnv$dataMat
  e$model$minTime = min(data[[method$time]])
  e$model$durTime = max(data[[method$time]]) - e$model$minTime
  e$runTime = as.numeric(Sys.time() - startTime)
  return(e)
})


setMethod('finalize', signature('clCrimCVMethodGBTM'), function(method, data, fitEnv) {
  assert_that(has_name(fitEnv$model, 'beta'), msg='invalid crimCV model returned from fit. The model either failed to initialize, converge, or its specification is unsupported.')

  model = new('clCrimCVModelGBTM',
              method=method,
              data=data,
              model=fitEnv$model,
              clusterNames=make.clusterNames(method$nClusters))
  return(model)
})
