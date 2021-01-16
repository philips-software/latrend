#' @include method.R

#' @name interface-longclust
#' @rdname interface-longclust
#' @title longclust interface
#' @seealso [lcMethodLongclust] \link[longclust]{longclust-package}
#' @keywords internal
NULL

setClass('lcMethodLongclust', contains = 'lcMatrixMethod')

#' @export
#' @title Specify Longclust method
#' @inheritParams lcMatrixMethod-class
#' @inheritParams lcMethodKML
#' @param ... Arguments passed to [longclust::longclustEM].
#' The following external arguments are ignored: data, x, Gmin, Gmax, userseed.
#' @examples
#' library(longclust)
#' data(latrendData)
#' method <- lcMethodLongclust("Y", id = "Id", time = "Time", nClusters = 3)
#' model <- latrend(method, latrendData)
#' @family lcMethod implementations
#' @references
#' \insertRef{mcnicholas2019longclust}{latrend}
lcMethodLongclust = function(response,
                             time = getOption('latrend.time'),
                             id = getOption('latrend.id'),
                             nClusters = 2,
                             ...) {
  lcMethod.call(
    'lcMethodLongclust',
    call = match.call.defaults(),
    defaults = longclust::longclustEM,
    excludeArgs = c('data', 'x', 'Gmin', 'Gmax', 'userseed')
  )
}

#' @rdname interface-longclust
#' @inheritParams getName
setMethod('getName', signature('lcMethodLongclust'), function(object) 'longclust')

#' @rdname interface-longclust
setMethod('getShortName', signature('lcMethodLongclust'), function(object) 'longclust')

#' @rdname interface-longclust
#' @inheritParams fit
setMethod('fit', signature('lcMethodLongclust'), function(method, data, envir, verbose, ...) {
  args = as.list(method, args = longclust::longclustEM)
  args$x = envir$dataMat
  args$Gmin = method$nClusters
  args$Gmax = method$nClusters
  if (hasName(method, 'seed')) {
    args$userseed = method$seed
  }

  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  suppressFun({
    model = do.call(longclust::longclustEM, args)
  })

  new(
    'lcModelLongclust',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
