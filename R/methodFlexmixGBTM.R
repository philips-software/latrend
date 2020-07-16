#' @include method.R
#' @include methodFlexmix.R
setClass('lcMethodFlexmixGBTM', contains = 'lcMethodFlexmix')

#' @export
#' @title Group-based trajectory modeling using flexmix
#' @description Fits a GBTM based on the [FLXMRglm] driver.
#' @inheritParams lcMethodLcmmGBTM
#' @inheritDotParams flexmix::flexmix
#' @inheritDotParams flexmix::FLXMRglm
#' @examples
#' model = latrend(lcMethodFlexmixGBTM(), data=testLongData)
#'
#' model = latrend(lcMethodFlexmixGBTM(Value ~ Time, family='Gamma'), data=copy(testLongData)[, Value := abs(Value)])
#' @family lcMethod package interfaces
lcMethodFlexmixGBTM = function(formula,
                               formula.mb =  ~ 1,
                               time = getOption('latrend.time'),
                               id = getOption('latrend.id'),
                               nClusters = 2,
                               ...) {
  lcMethod.call(
    'lcMethodFlexmixGBTM',
    call = match.call.defaults(),
    defaults = c(flexmix::flexmix, flexmix::FLXMRglm),
    excludeArgs = c('data', 'k')
  )
}

setMethod('getName', signature('lcMethodFlexmixGBTM'), function(object) 'group-based trajectory model')

setMethod('getShortName', signature('lcMethodFlexmixGBTM'), function(object) 'gbtm')

setMethod('preFit', signature('lcMethodFlexmixGBTM'), function(method, data, envir, verbose) {
  e = callNextMethod()
  e$model = flexmix::FLXMRglm(family = method$family)
  return(e)
})
