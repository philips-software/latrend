#' @include clMethod.R
setClass('clMethodMixtoolsNPRM', contains='clMatrixMethod')

#' @export
#' @importFrom mixtools npEM
#' @title Specify non-parametric estimation for independent repeated measures
#' @inheritParams clMatrixMethod
#' @inheritParams mixtools::npEM
#' @examples
#' method = clMethodMixtoolsNPRM(Measurement ~ 1,
#'                      time='Assessment',
#'                      id='Id', nClusters=3)
#' @family clMethod classes
clMethodMixtoolsNPRM = function(formula=Value ~ 1,
                             time=getOption('cluslong.time'),
                             id=getOption('cluslong.id'),
                             nClusters=2,
                             maxiter=500,
                             eps=1e-8,
                             ...) {
  new('clMethodMixtoolsNPRM', call=match.call.defaults())
}

setMethod('getName', signature('clMethodMixtoolsNPRM'), function(object) 'non-parametric estimation for independent repeated measurements using mixtools')

setMethod('getName0', signature('clMethodMixtoolsNPRM'), function(object) 'nprm')


setMethod('fit', signature('clMethodMixtoolsNPRM'), function(method, data, envir, verbose, ...) {
  e = new.env(parent=envir)

  args = as.list(method)
  args$x = envir$dataMat
  args$mu0 = method$nClusters
  args$verb = canShow(verbose, 'fine')
  args[setdiff(names(args), formalArgs(npEM))] = NULL #remove undefined arguments

  # Helper variables
  valueColumn = formula(method) %>% getResponse
  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  startTime = Sys.time()
  suppressFun({
    e$model = do.call(npEM, args)
  })
  e$runTime = as.numeric(Sys.time() - startTime)
  return(e)
})


setMethod('finalize', signature('clMethodMixtoolsNPRM'), function(method, data, envir, verbose, ...) {
    model = new('clModelMixtoolsRM',
                method=method,
                data=data,
                model=envir$model,
                clusterNames=make.clusterNames(method$nClusters))
    return(model)
  })