#' @include method.R
#' @include methodFlexmix.R
setClass('lcMethodFlexmixGBTM', contains = 'lcMethodFlexmix')

#' @export
#' @title Group-based trajectory modeling using flexmix
#' @description Fits a GBTM based on the [flexmix::FLXMRglm] driver.
#' @inheritParams lcMethodFlexmix
#' @param ... Arguments passed to [flexmix::flexmix] or [flexmix::FLXMRglm].
#' The following arguments are ignored: data, k, trace.
#' @examples
#' data(latrendData)
#' if (require("flexmix")) {
#'   method <- lcMethodFlexmixGBTM(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
#'   model <- latrend(method, latrendData)
#' }
#' @family lcMethod package interfaces
#' @references
#' \insertRef{gruen2008flexmix}{latrend}
lcMethodFlexmixGBTM = function(
  formula,
  formula.mb =  ~ 1,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  nClusters = 2,
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodFlexmixGBTM'
  do.call(new, as.list(mc))
}


#' @rdname interface-flexmix
setMethod('getArgumentDefaults', 'lcMethodFlexmixGBTM', function(object) {
  c(
    formals(lcMethodFlexmixGBTM),
    formals(flexmix::flexmix),
    formals(flexmix::FLXMRglm),
    callNextMethod()
  )
})

#' @rdname interface-flexmix
setMethod('getArgumentExclusions', 'lcMethodFlexmixGBTM', function(object) {
  union(
    callNextMethod(),
    c('data', 'k')
  )
})

#' @rdname interface-flexmix
setMethod('getName', 'lcMethodFlexmixGBTM', function(object) 'group-based trajectory model')

#' @rdname interface-flexmix
setMethod('getShortName', 'lcMethodFlexmixGBTM', function(object) 'gbtm')

#' @rdname interface-flexmix
setMethod('preFit', 'lcMethodFlexmixGBTM', function(method, data, envir, verbose) {
  e = callNextMethod()
  e$model = flexmix::FLXMRglm(family = method$family)
  return(e)
})
