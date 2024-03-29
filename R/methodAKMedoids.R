#' @include method.R methodMatrix.R

#' @name interface-akmedoids
#' @title akmedoids interface
#' @seealso [lcMethodAkmedoids] `akmedoids::akclustr`
#' @keywords internal
NULL

setClass('lcMethodAkmedoids', contains = 'lcMatrixMethod')

#' @export
#' @title Specify AKMedoids method
#' @inheritParams lcMatrixMethod-class
#' @inheritParams lcMethodFunction
#' @inheritParams lcMethodKML
#' @param clusterCenter A function for computing the cluster center representation.
#' @param crit Criterion to apply for internal model selection. Not applicable.
#' @param ... Arguments passed to `akmedoids::akclustr`.
#' The following external arguments are ignored: traj, id_field, k
#' @examples
#' data(latrendData)
#' if (rlang::is_installed("akmedoids")) {
#'   method <- lcMethodAkmedoids(response = "Y", time = "Time", id = "Id", nClusters = 3)
#'   model <- latrend(method, data = latrendData)
#' }
#' @family lcMethod implementations
#' @references
#' \insertRef{adepeju2020akmedoids}{latrend}
lcMethodAkmedoids = function(
  response,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  nClusters = 3, # must be > 2
  clusterCenter = median,
  crit = 'Calinski_Harabasz', # Default silhouette width results in: Error in smooth.spline(x, y) : 'tol' must be strictly positive and finite
  ...
) {
  .loadOptionalPackage('akmedoids')
  mc = match.call.all()
  mc$Class = 'lcMethodAkmedoids'
  do.call(new, as.list(mc))
}

#' @rdname interface-akmedoids
setMethod('getArgumentDefaults', 'lcMethodAkmedoids', function(object) {
  c(
    formals(lcMethodAkmedoids),
    formals(akclustr),
    callNextMethod()
  )
})

#' @rdname interface-akmedoids
setMethod('getArgumentExclusions', 'lcMethodAkmedoids', function(object) {
  union(
    callNextMethod(),
    c('traj', 'id_field', 'k')
  )
})

#' @rdname interface-akmedoids
setMethod('getCitation', 'lcMethodAkmedoids', function(object, ...) {
  citation('akmedoids')
})

#' @rdname interface-akmedoids
setMethod('getName', 'lcMethodAkmedoids', function(object) 'anchored k-medoids')

#' @rdname interface-akmedoids
setMethod('getShortName', 'lcMethodAkmedoids', function(object) 'akm')

#' @rdname interface-akmedoids
#' @inheritParams fit
setMethod('fit', 'lcMethodAkmedoids', function(method, data, envir, verbose, ...) {
  args = as.list(method, args = akclustr)
  args$traj = envir$dataMat
  args$k = method$nClusters
  args$id_field = FALSE

  assert_that(method$nClusters %between% c(3, 20), msg = 'akmedoids only supports nClusters = 3, ..., 20')

  model = do.call(akclustr, args)

  clusNames = make.clusterNames(method$nClusters)

  trajAssignments = model$solutions[[1]]
  assert_that(length(trajAssignments) > 0, msg = 'akmedoids package returned an unexpected result: no membership output returned')

  lcModelPartition(
    data,
    trajectoryAssignments = factor(
      trajAssignments,
      levels = LETTERS[1:method$nClusters],
      labels = clusNames
    ),
    center = method$clusterCenter,
    response = responseVariable(method),
    time = timeVariable(method),
    id = idVariable(method),
    clusterNames = clusNames,
    method = method,
    model = model,
    name = 'akmedoids'
  )
})
