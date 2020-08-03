#' @include method.R
setClass('lcMethodFlexmix', contains = 'lcMethod')

#' @export
#' @title Method interface to flexmix()
#' @description Wrapper to the `flexmix()` method from the `flexmix` package.
#' @inheritDotParams flexmix::flexmix
#' @examples
#'
#' @family lcMethod package interfaces
lcMethodFlexmix = function(formula,
                           formula.mb =  ~ 1,
                           time = getOption('latrend.time'),
                           id = getOption('latrend.id'),
                           nClusters = 2,
                           ...) {
  lcMethod.call(
    'lcMethodFlexmix',
    call = match.call.defaults(),
    defaults = flexmix::flexmix,
    excludeArgs = c('data', 'concomitant', 'k')
  )
}

setMethod('getName', signature('lcMethodFlexmix'), function(object) 'flexmix')

setMethod('getShortName', signature('lcMethodFlexmix'), function(object) 'flx')


setMethod('preFit', signature('lcMethodFlexmix'), function(method, data, envir, verbose, ...) {
  e = new.env()

  f = formula(method) %>% dropRE %>% dropCLUSTER
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


setMethod('fit', signature('lcMethodFlexmix'), function(method, data, envir, verbose, ...) {
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