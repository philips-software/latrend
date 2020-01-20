#' @include clMethod.R
#' @include clFlexmixMethod.R
setClass('clFlexmixMethodGBTM', contains='clFlexmixMethod')

#' @export
#' @title Group-based trajectory modeling using flexmix
#' @description Fits a GBTM based on the [FLXMRglm] driver.
#' @inheritParams clMethodGBTM
#' @inheritParams flexmix::FLXMRglm
#' @examples
#' model = cluslong(clFlexmixMethodGBTM(), data=testLongData)
#'
#' model = cluslong(clFlexmixMethodGBTM(Value ~ Time, family='Gamma'), data=copy(testLongData)[, Value := abs(Value)])
#' @family clMethod package interfaces
clFlexmixMethodGBTM = function(formula=Value ~ 1,
                           formula.mb=~1,
                           time=getOption('cluslong.time'),
                           id=getOption('cluslong.id'),
                           nClusters=2,
                           family='gaussian',
                           control=NULL) {
  new('clFlexmixMethodGBTM', call=match.call.defaults())
}

setMethod('getName', signature('clFlexmixMethodGBTM'), function(object) 'group-based trajectory model')

setMethod('getName0', signature('clFlexmixMethodGBTM'), function(object) 'gbtm')

#' @importFrom flexmix FLXMRglm
setMethod('prepare', signature('clFlexmixMethodGBTM'), function(method, data) {
  e = callNextMethod()
  e$model = FLXMRglm(family=method$family)
  return(e)
})
