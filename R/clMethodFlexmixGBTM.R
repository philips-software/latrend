#' @include clMethod.R
#' @include clMethodFlexmix.R
setClass('clMethodFlexmixGBTM', contains='clMethodFlexmix')

#' @export
#' @title Group-based trajectory modeling using flexmix
#' @description Fits a GBTM based on the [FLXMRglm] driver.
#' @inheritParams clMethodGBTM
#' @inheritParams flexmix::FLXMRglm
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
                           family='gaussian',
                           control=NULL) {
  new('clMethodFlexmixGBTM', call=match.call.defaults())
}

setMethod('getName', signature('clMethodFlexmixGBTM'), function(object) 'group-based trajectory model')

setMethod('getName0', signature('clMethodFlexmixGBTM'), function(object) 'gbtm')

#' @importFrom flexmix FLXMRglm
setMethod('prepare', signature('clMethodFlexmixGBTM'), function(method, data, verbose) {
  e = callNextMethod()
  e$model = FLXMRglm(family=method$family)
  return(e)
})
