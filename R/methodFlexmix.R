#' @include method.R

#' @name interface-flexmix
#' @rdname interface-flexmix
#' @title flexmix interface
#' @seealso [lcMethodFlexmix] \link[flexmix]{flexmix}
#' @keywords internal
NULL

setClass('lcMethodFlexmix', contains = 'lcMethod')

#' @export
#' @title Method interface to flexmix()
#' @description Wrapper to the `flexmix()` method from the `flexmix` package.
#' @inheritParams lcMethodKML
#' @param formula A `formula` specifying the model.
#' @param formula.mb A `formula` specifying the class membership model. By default, an intercept-only model is used.
#' @param ... Arguments passed to [flexmix::flexmix].
#' The following arguments are ignored: data, concomitant, k.
#' @examples
#' data(latrendData)
#' if (require("flexmix")) {
#'   method <- lcMethodFlexmix(Y ~ Time, id = "Id", time = "Time", nClusters = 3)
#'   model <- latrend(method, latrendData)
#' }
#' @family lcMethod package interfaces
#' @references
#' \insertRef{gruen2008flexmix}{latrend}
lcMethodFlexmix = function(
  formula,
  formula.mb =  ~ 1,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  nClusters = 2,
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodFlexmix'
  do.call(new, as.list(mc))
}

#' @rdname interface-flexmix
setMethod('getArgumentDefaults', 'lcMethodFlexmix', function(object) {
  c(
    formals(lcMethodFlexmix),
    formals(flexmix::flexmix),
    callNextMethod()
  )
})

#' @rdname interface-flexmix
setMethod('getArgumentExclusions', 'lcMethodFlexmix', function(object) {
  union(
    callNextMethod(),
    c('data', 'concomitant', 'k')
  )
})

#' @rdname interface-flexmix
setMethod('getName', 'lcMethodFlexmix', function(object) 'flexmix')

#' @rdname interface-flexmix
setMethod('getShortName', 'lcMethodFlexmix', function(object) 'flx')

#' @rdname interface-flexmix
setMethod('preFit', 'lcMethodFlexmix', function(method, data, envir, verbose, ...) {
  e = new.env()

  f = formula(method) %>% dropRE()
  e$formula = paste(deparse(f), '|', idVariable(method)) %>% as.formula
  if (isArgDefined(method, 'model')) {
    e$model = method$model
  }

  cat(verbose, sprintf('\tformula: %s', deparse(e$formula)), level = verboseLevels$finest)

  # drop intercept from formula.mb
  if (hasCovariates(method$formula.mb)) {
    e$concomitant = flexmix::FLXPmultinom(formula = method$formula.mb)
  } else {
    e$concomitant = flexmix::FLXPconstant()
  }

  return(e)
})

#' @rdname interface-flexmix
#' @inheritParams fit
setMethod('fit', 'lcMethodFlexmix', function(method, data, envir, verbose, ...) {
  args = as.list(method, args = flexmix::flexmix)
  args$data = data
  args$formula = envir$formula
  args$model = envir$model
  args$concomitant = envir$formula.mb
  args$k = method$nClusters

  if (is.null(args$model)) {
    # flexmix signature not found for call with `model=NULL`
    args$model = NULL
  }

  flexmodel = do.call(flexmix::flexmix, args)

  if (flexmodel@k < method$nClusters) {
    warning('flexmix returned a result with fewer components than was specified for nClusters')
  }

  new(
    'lcModelFlexmix',
    method = method,
    data = data,
    model = flexmodel,
    clusterNames = make.clusterNames(flexmodel@k)
  )
})
