#' @include method.R
#' @include methodMatrix.R

#' @name interface-akmedoids
#' @rdname interface-akmedoids
#' @title akmedoids interface
#' @seealso [lcMethodAkmedoids] \link[akmedoids]{akclustr}
#' @keywords internal
NULL

setClass('lcMethodAkmedoids', contains = 'lcMatrixMethod')

#' @export
#' @title Specify AKMedoids method
#' @inheritParams lcMatrixMethod-class
#' @inheritParams lcMethodCustom
#' @inheritParams lcMethodKML
#' @param clusterCenter A function for computing the cluster center representation.
#' @param crit Criterion to apply for internal model selection. Not applicable.
#' @param ... Arguments passed to [akmedoids::akclustr].
#' The following external arguments are ignored: traj, id_field, k
#' @examples
#' library(akmedoids)
#' data(latrendData)
#' method <- lcMethodAkmedoids(response = "Y", time = "Time", id = "Id", nClusters = 3)
#' model <- latrend(method, data = latrendData)
#' @family lcMethod implementations
#' @references
#' \insertRef{adepeju2020akmedoids}{latrend}
lcMethodAkmedoids = function(response,
                             time = getOption('latrend.time'),
                             id = getOption('latrend.id'),
                             nClusters = 3, # must be > 2
                             clusterCenter = median,
                             crit = 'Calinski_Harabasz', # Default silhouette width results in: Error in smooth.spline(x, y) : 'tol' must be strictly positive and finite
                             ...) {
  lcMethod.call(
    'lcMethodAkmedoids',
    call = match.call.defaults(),
    defaults = akmedoids::akclustr,
    excludeArgs = c('traj', 'id_field', 'k')
  )
}

#' @rdname interface-akmedoids
setMethod('getName', signature('lcMethodAkmedoids'), function(object) 'anchored k-medoids')

#' @rdname interface-akmedoids
setMethod('getShortName', signature('lcMethodAkmedoids'), function(object) 'akm')

#' @rdname interface-akmedoids
#' @inheritParams fit
setMethod('fit', signature('lcMethodAkmedoids'), function(method, data, envir, verbose, ...) {
  args = as.list(method, args = akmedoids::akclustr)
  args$traj = envir$dataMat
  args$k = method$nClusters
  args$id_field = FALSE

  # Helper variables
  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  suppressFun({
    model = do.call(akmedoids::akclustr, args)
  })

  clusNames = make.clusterNames(method$nClusters)

  trajAssignments = model$solutions[[1]]
  assert_that(length(trajAssignments) > 0, msg = 'no membership output returned')

  lcModelCustom(
    data,
    trajectoryAssignments = factor(trajAssignments,
      levels = LETTERS[1:method$nClusters],
      labels = clusNames),
    clusterTrajectories = method$clusterCenter,
    response = responseVariable(method),
    time = timeVariable(method),
    id = idVariable(method),
    clusterNames = clusNames,
    converged = TRUE,
    method = method,
    model = model,
    name = 'akmedoids'
  )
})
