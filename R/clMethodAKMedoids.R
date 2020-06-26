#' @include clMethod.R
setClass('clMethodAKMedoids', contains = 'clMatrixMethod')

#' @export
#' @title Specify AKMedoids method
#' @inheritParams clMatrixMethod
#' @inheritParams clMethodCustom
#' @param ... Arguments passed to \link[akmedoids]{akmedoids.clust}.
#' @examples
#' method = clMethodAKMedoids(HoursOfUse ~ 0,
#'                      time='Day',
#'                      id='Patient', nClusters=3)
#' cluslong(method, data=OSA1y)
#' @family clMethod implementations
clMethodAKMedoids = function(response = getOption('cluslong.response'),
                             time = getOption('cluslong.time'),
                             id = getOption('cluslong.id'),
                             nClusters = 3,
                             clusterCenter = median,
                             ...) {
  clMethod.call(
    'clMethodAKMedoids',
    call = match.call.defaults(),
    defaults = akmedoids::akmedoids.clust,
    excludeArgs = c('traj', 'id_field', 'k')
  )
}

setMethod('getName', signature('clMethodAKMedoids'), function(object) 'anchored k-medoids')

setMethod('getShortName', signature('clMethodAKMedoids'), function(object) 'akm')


setMethod('fit', signature('clMethodAKMedoids'), function(method, data, envir, verbose, ...) {
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

  clModelCustom(
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
