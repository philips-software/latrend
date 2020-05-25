#' @include clMethod.R
setClass('clMethodMixtoolsNPRM', contains='clMatrixMethod')

#' @export
#' @importFrom mixtools npEM
#' @title Specify non-parametric estimation for independent repeated measures
#' @inheritParams clMatrixMethod
#' @inheritDotParams mixtools::npEM
#' @examples
#' method = clMethodMixtoolsNPRM(Measurement ~ 0,
#'                      time='Assessment',
#'                      id='Id', nClusters=3)
#' @family clMethod implementations
clMethodMixtoolsNPRM = function(formula=Value ~ 0,
                             time=getOption('cluslong.time'),
                             id=getOption('cluslong.id'),
                             nClusters=2,
                             ...
) {
  clMethod('clMethodMixtoolsNPRM', call=match.call.defaults(),
           defaults=longclust::longclustEM,
           excludeArgs=c('data', 'x', 'mu0', 'verb'))
}

setMethod('getName', signature('clMethodMixtoolsNPRM'), function(object) 'non-parametric estimation for independent repeated measurements using mixtools')

setMethod('getName0', signature('clMethodMixtoolsNPRM'), function(object) 'nprm')


setMethod('fit', signature('clMethodMixtoolsNPRM'), function(method, data, envir, verbose, ...) {
  args = as.list(method)
  args$x = envir$dataMat
  args$mu0 = method$nClusters
  args$verb = canShow(verbose, 'fine')
  args[setdiff(names(args), formalArgs(npEM))] = NULL #remove undefined arguments

  # Helper variables
  valueColumn = formula(method) %>% getResponse
  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  suppressFun({
    model = do.call(npEM, args)
  })

  new('clModelMixtoolsRM',
      method=method,
      data=data,
      model=model,
      clusterNames=make.clusterNames(method$nClusters))
})
