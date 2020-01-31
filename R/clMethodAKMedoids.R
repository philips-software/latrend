#' @include clMethod.R
setClass('clMethodAKMedoids', contains='clMatrixMethod')

#' @export
#' @importFrom akmedoids akmedoids.clust
#' @title Specify KML method
#' @inheritParams clMatrixMethod
#' @inheritParams akmedoids::akmedoids.clust
#' @examples
#' method = clMethodAKMedoids(Measurement ~ 1,
#'                      time='Assessment',
#'                      id='Id', nClusters=3)
#' @family clMethod classes
clMethodAKMedoids = function(formula=Value ~ 1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=3,
                       method='linear',
                       clusterCenter=median) {
  new('clMethodAKMedoids', call=match.call.defaults())
}

setMethod('getName', signature('clMethodAKMedoids'), function(object) 'anchored k-medoids')

setMethod('getName0', signature('clMethodAKMedoids'), function(object) 'akm')


setMethod('fit', signature('clMethodAKMedoids'), function(method, data, prepEnv) {
  e = new.env(parent=prepEnv)

  args = as.list(method)
  args$traj = prepEnv$dataMat
  args$k = method$nClusters
  args$id_field = FALSE
  args[setdiff(names(args), formalArgs(akmedoids.clust))] = NULL #remove undefined arguments

  # Helper variables
  valueColumn = formula(method) %>% getResponse
  suppressFun = if(canShowModelOutput()) force else capture.output

  startTime = Sys.time()
  suppressFun({
    e$model = do.call(akmedoids.clust, args)
  })
  e$runTime = as.numeric(Sys.time() - startTime)
  return(e)
})


setMethod('finalize', signature('clMethodAKMedoids'), function(method, data, fitEnv) {
  clusNames = make.clusterNames(method$nClusters)

  clModelCustom(data, clusterAssignments=factor(fitEnv$model$memberships, levels=LETTERS[1:method$nClusters], labels=clusNames),
                clusterTrajectories=method$clusterCenter,
                response=getResponse(method$formula),
                time=method$time,
                id=method$id,
                clusterNames=clusNames,
                converged=TRUE,
                method=method,
                model=fitEnv$model,
                name='akmedoids')
})