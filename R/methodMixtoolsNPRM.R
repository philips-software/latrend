#' @include method.R
setClass('lcMethodMixtoolsNPRM', contains = 'lcMatrixMethod')

#' @export
#' @title Specify non-parametric estimation for independent repeated measures
#' @inheritParams lcMatrixMethod
#' @inheritDotParams mixtools::npEM
#' @examples
#' method = lcMethodMixtoolsNPRM(response = 'Measurement',
#'                      time='Assessment',
#'                      id='Id', nClusters=3)
#' @family lcMethod implementations
lcMethodMixtoolsNPRM = function(response,
                                time = getOption('latrend.time'),
                                id = getOption('latrend.id'),
                                nClusters = 2,
                                blockid = NULL,
                                bw = NULL,
                                h = NULL,
                                ...) {
  lcMethod.call(
    'lcMethodMixtoolsNPRM',
    call = match.call.defaults(),
    defaults = mixtools::npEM,
    excludeArgs = c('data', 'x', 'mu0', 'verb')
  )
}

setMethod('getName', signature('lcMethodMixtoolsNPRM'), function(object) 'non-parametric estimation for independent repeated measurements using mixtools')

setMethod('getShortName', signature('lcMethodMixtoolsNPRM'), function(object) 'nprm')

setMethod('fit', signature('lcMethodMixtoolsNPRM'), function(method, data, envir, verbose, ...) {
  args = as.list(method, args = mixtools::npEM)
  args$x = envir$dataMat
  args$mu0 = method$nClusters
  args$verb = canShow(verbose, 'fine')

  if(is.null(args$blockid)) { args$blockid = NULL }
  if(is.null(args$h)) { args$h = NULL }
  if(is.null(args$bw)) { args$bw = NULL }

  # Helper variables
  valueColumn = method$response
  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  suppressFun({
    model = do.call(mixtools::npEM, args)
  })

  new(
    'lcModelMixtoolsRM',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})