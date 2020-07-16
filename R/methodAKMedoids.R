#' @include method.R
#' @include methodMatrix.R
setClass('lcMethodAKMedoids', contains = 'lcMatrixMethod')

#' @export
#' @title Specify AKMedoids method
#' @inheritParams lcMatrixMethod
#' @inheritParams lcMethodCustom
#' @param ... Arguments passed to \link[akmedoids]{akmedoids.clust}.
#' @examples
#' method = lcMethodAKMedoids(HoursOfUse ~ 0,
#'                      time='Day',
#'                      id='Patient', nClusters=3)
#' latrend(method, data=OSA1y)
#' @family lcMethod implementations
lcMethodAKMedoids = function(response,
                             time = getOption('latrend.time'),
                             id = getOption('latrend.id'),
                             nClusters = 3,
                             clusterCenter = median,
                             ...) {
  lcMethod.call(
    'lcMethodAKMedoids',
    call = match.call.defaults(),
    defaults = akmedoids::akmedoids.clust,
    excludeArgs = c('traj', 'id_field', 'k')
  )
}

setMethod('getName', signature('lcMethodAKMedoids'), function(object) 'anchored k-medoids')

setMethod('getShortName', signature('lcMethodAKMedoids'), function(object) 'akm')


setMethod('fit', signature('lcMethodAKMedoids'), function(method, data, envir, verbose, ...) {
  args = as.list(method, args = akmedoids::akmedoids.clust)
  args$traj = envir$dataMat
  args$k = method$nClusters
  args$id_field = FALSE

  # Helper variables
  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  suppressFun({
    model = do.call(akmedoids::akmedoids.clust, args)
  })

  clusNames = make.clusterNames(method$nClusters)

  assert_that(!is.null(model$membership), msg = 'no membership output returned')

  lcModelCustom(
    data,
    clusterAssignments = factor(model$membership, levels = LETTERS[1:method$nClusters], labels =
                                  clusNames),
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
