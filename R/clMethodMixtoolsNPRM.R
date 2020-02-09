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


setMethod('fit', signature('clMethodMixtoolsNPRM'), function(method, data, prepEnv) {
  e = new.env(parent=prepEnv)

  args = as.list(method)
  args$x = prepEnv$dataMat
  args$mu0 = method$nClusters
  args$verb = canShowModelOutput('FINE')
  args[setdiff(names(args), formalArgs(npEM))] = NULL #remove undefined arguments

  # Helper variables
  valueColumn = formula(method) %>% getResponse
  suppressFun = if(canShowModelOutput()) force else capture.output

  startTime = Sys.time()
  suppressFun({
    e$model = do.call(npEM, args)
  })
  e$runTime = as.numeric(Sys.time() - startTime)
  return(e)
})


setMethod('finalize', signature('clMethodMixtoolsNPRM'), function(method, data, fitEnv) {
    model = new('clModelMixtoolsRM',
                method=method,
                data=data,
                model=fitEnv$model,
                clusterNames=make.clusterNames(method$nClusters))
    return(model)
  })