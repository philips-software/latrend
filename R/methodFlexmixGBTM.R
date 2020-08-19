#' @include method.R
#' @include methodFlexmix.R
setClass('lcMethodFlexmixGBTM', contains = 'lcMethodFlexmix')

#' @export
#' @title Group-based trajectory modeling using flexmix
#' @description Fits a GBTM based on the [FLXMRglm] driver.
#' @inheritParams lcMethodFlexmix
#' @param ... Arguments passed to [flexmix::flexmix] or [flexmix::FLXMRglm].
#' The following arguments are ignored: data, k, trace.
#' @examples
#' data(testLongData)
#' method <- lcMethodFlexmixGBTM()
#' model <- latrend(method, testLongData)
#'
#' method <- lcMethodFlexmixGBTM(Value ~ Time, family = "Gamma")
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
