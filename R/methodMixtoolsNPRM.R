#' @include method.R
setClass('lcMethodMixtoolsNPRM', contains = 'lcMatrixMethod')

#' @export
#' @title Specify non-parametric estimation for independent repeated measures
#' @inheritParams lcMatrixMethod-class
#' @inheritParams lcMethodKML
#' @param blockid See [mixtools::npEM].
#' @param bw See [mixtools::npEM].
#' @param h See [mixtools::npEM].
#' @param ... Arguments passed to [mixtools::npEM].
#' The following optional arguments are ignored: data, x, mu0, verb.
#' @examples
#' library(mixtools)
#' data(latrendData)
#' method <- lcMethodMixtoolsNPRM("Y", id = "Id", time = "Time", nClusters = 3)
#' model <- latrend(method, latrendData)
#' @family lcMethod implementations
#' @references
#' \insertRef{benaglia2009mixtools}{latrend}
lcMethodMixtoolsNPRM = function(
  response,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  nClusters = 2,
  blockid = NULL,
  bw = NULL,
  h = NULL,
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodMixtoolsNPRM'
  do.call(new, as.list(mc))
}

#' @rdname interface-mixtools
setMethod('getArgumentDefaults', signature('lcMethodMixtoolsNPRM'), function(object) {
  c(
    formals(lcMethodMixtoolsNPRM),
    formals(mixtools::npEM),
    callNextMethod()
  )
})

#' @rdname interface-mixtools
setMethod('getArgumentExclusions', signature('lcMethodMixtoolsNPRM'), function(object) {
  union(
    callNextMethod(),
    c('data', 'x', 'mu0', 'verb')
  )
})

#' @rdname interface-mixtools
setMethod('getName', signature('lcMethodMixtoolsNPRM'), function(object) 'non-parametric estimation for independent repeated measurements using mixtools')

#' @rdname interface-mixtools
setMethod('getShortName', signature('lcMethodMixtoolsNPRM'), function(object) 'nprm')

#' @rdname interface-mixtools
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

  model = do.call(mixtools::npEM, args)

  new(
    'lcModelMixtoolsRM',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
