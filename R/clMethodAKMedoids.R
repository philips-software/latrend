#' @include clMethod.R
setClass('clMethodAKMedoids', contains='clMatrixMethod')

#' @export
#' @importFrom akmedoids akmedoids.clust
#' @title Specify KML method
#' @inheritParams clMatrixMethod
#' @inheritParams clMethodCustom
#' @inheritDotParams akmedoids::akmedoids.clust
#' @examples
#' method = clMethodAKMedoids(Measurement ~ 0,
#'                      time='Assessment',
#'                      id='Id', nClusters=3)
#' @family clMethod classes
clMethodAKMedoids = function(formula=Value ~ 0,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=3,
                       clusterCenter=median,
                       ...
) {
  clMethod('clMethodAKMedoids', call=match.call.defaults(),
           defaults=akmedoids::akmedoids.clust,
           excludeArgs=c('traj', 'id_field', 'k'))
}

setMethod('getName', signature('clMethodAKMedoids'), function(object) 'anchored k-medoids')

setMethod('getName0', signature('clMethodAKMedoids'), function(object) 'akm')


setMethod('fit', signature('clMethodAKMedoids'), function(method, data, envir, verbose, ...) {
  e = new.env(parent=envir)

  args = as.list(method, fun=akmedoids.clust)
  args$traj = envir$dataMat
  args$k = method$nClusters
  args$id_field = FALSE

  # Helper variables
  valueColumn = formula(method) %>% getResponse
  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  suppressFun({
    e$model = do.call(akmedoids.clust, args)
  })
  return(e)
})


setMethod('finalize', signature('clMethodAKMedoids'), function(method, data, envir, verbose, ...) {
  clusNames = make.clusterNames(method$nClusters)

  clModelCustom(data, clusterAssignments=factor(envir$model$memberships, levels=LETTERS[1:method$nClusters], labels=clusNames),
                clusterTrajectories=method$clusterCenter,
                response=getResponse(method$formula),
                time=method$time,
                id=method$id,
                clusterNames=clusNames,
                converged=TRUE,
                method=method,
                model=envir$model,
                name='akmedoids')
})