#' @include clMethod.R
setClass('clMethodFlexmix', contains='clMethod')

#' @export
#' @importFrom flexmix flexmix FLXPconstant
#' @title Method interface to flexmix()
#' @description Wrapper to the `flexmix()` method from the `flexmix` package.
#' @inheritParams flexmix::flexmix
#' @examples
#'
#' @family clMethod package interfaces
clMethodFlexmix = function(formula=Value ~ 1,
                        formula.mb=~1,
                        time=getOption('cluslong.time'),
                        id=getOption('cluslong.id'),
                        nClusters=2,
                        model=NULL,
                        control=NULL) {
  new('clMethodFlexmix', call=match.call.defaults())
}

setMethod('getName', signature('clMethodFlexmix'), function(object) 'flexmix')

setMethod('getName0', signature('clMethodFlexmix'), function(object) 'flx')


setMethod('prepare', signature('clMethodFlexmix'), function(method, data) {
  e = new.env()

  f = formula(method) %>% dropRE %>% dropCLUSTER
  e$formula = paste(deparse(f), '|', method$id) %>% as.formula
  if(isArgDefined(method, 'model')) {
    e$model = method$model
  }

  logfinest(sprintf('\tformula: %s', deparse(e$formula)))

  # drop intercept from formula.mb
  if(hasCovariates(method$formula.mb)) {
    e$concomitant = FLXPmultinom(formula=method$formula.mb)
  } else {
    e$concomitant = FLXPconstant()
  }

  return(e)
})


setMethod('fit', signature('clMethodFlexmix'), function(method, data, prepEnv) {
  e = new.env(parent=prepEnv)

  args = as.list(method)
  args$data = data
  args$formula = prepEnv$formula
  args$model = prepEnv$model
  args$concomitant = prepEnv$formula.mb
  args$k = method$nClusters
  args[setdiff(names(args), formalArgs(flexmix))] = NULL #remove undefined arguments

  if(is.null(args$model)) {
    # flexmix signature not found for call with `model=NULL`
    args$model = NULL
  }

  startTime = Sys.time()

  model = do.call(flexmix, args)

  e$runTime = as.numeric(Sys.time() - startTime)

  e$model = model
  return(e)
})


setMethod('finalize', signature('clMethodFlexmix'), function(method, data, fitEnv) {
  if(fitEnv$model@k < method$nClusters) {
    warning('flexmix returned a result with fewer components than was specified for nClusters')
  }
  model = new('clModelFlexmix',
              method=method,
              data=data,
              model=fitEnv$model,
              clusterNames=make.clusterNames(fitEnv$model@k))
  return(model)
})