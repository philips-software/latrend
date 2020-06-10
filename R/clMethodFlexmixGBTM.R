#' @include clMethod.R
#' @include clMethodFlexmix.R
setClass('clMethodFlexmixGBTM', contains='clMethodFlexmix')

#' @export
#' @title Group-based trajectory modeling using flexmix
#' @description Fits a GBTM based on the [FLXMRglm] driver.
#' @inheritParams clMethodLcmmGBTM
#' @inheritDotParams flexmix::flexmix
#' @inheritDotParams flexmix::FLXMRglm
#' @examples
#' model = cluslong(clMethodFlexmixGBTM(), data=testLongData)
#'
#' model = cluslong(clMethodFlexmixGBTM(Value ~ Time, family='Gamma'), data=copy(testLongData)[, Value := abs(Value)])
#' @family clMethod package interfaces
clMethodFlexmixGBTM = function(formula=Value ~ 1,
                           formula.mb=~1,
                           time=getOption('cluslong.time'),
                           id=getOption('cluslong.id'),
                           nClusters=2,
                           ...
) {
  .clMethod('clMethodFlexmixGBTM', call=match.call.defaults(),
           defaults=c(flexmix::flexmix, flexmix::FLXMRglm),
           excludeArgs=c('data', 'k'))
}

setMethod('getName', signature('clMethodFlexmixGBTM'), function(object) 'group-based trajectory model')

setMethod('getShortName', signature('clMethodFlexmixGBTM'), function(object) 'gbtm')

#' @importFrom flexmix FLXMRglm
setMethod('preFit', signature('clMethodFlexmixGBTM'), function(method, data, envir, verbose) {
  e = callNextMethod()
  e$model = flexmix::FLXMRglm(family=method$family)
  return(e)
})
