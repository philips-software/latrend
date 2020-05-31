#' @include clApproxModel.R
setClass('clModelPartition',
         representation(center='function',
                        clusterTrajectories='data.frame',
                        postprob='matrix',
                        name='character'),
         contains='clApproxModel')

#' @export
#' @title Create a clModel with pre-defined partitioning
#' @description Represents an arbitrary partitioning of a set of trajectories.
#' As such, this model has no predictive capabilities. The cluster trajectories are represented by the specified center function (mean by default).
#' @param clusterAssignments A `vector` of cluster membership per trajectory, either `factor`, or `integer` (`1` to `nClusters`).
#' @param nClusters The number of clusters. Optional for `factor` assignments.
#' @param clusterNames The names of the clusters, or a function with input `n` outputting a `character vector` of names.
clModelPartition = function(data,
                            clusterAssignments,
                            nClusters=NA,
                            center=meanNA,
                            clusterNames=NULL,
                            response=getOption('cluslong.response'),
                            time=getOption('cluslong.time'),
                            id=getOption('cluslong.id'),
                            name='part') {
  assert_that(is.data.frame(data),
              has_name(data, response),
              has_name(data, time),
              has_name(data, id))
  assert_that(is.character(clusterNames) || is.null(clusterNames),
              length(clusterNames) %in% c(0, nClusters))
  assert_that(is.function(center))
  assert_that(all(vapply(clusterAssignments, is.count, FUN.VALUE=TRUE)) || is.factor(clusterAssignments),
              length(clusterAssignments) == uniqueN(data[[id]]))

  if(is.factor(clusterAssignments)) {
    assert_that(is.na(nClusters) || nlevels(clusterAssignments) == nClusters)
  }
  intAssignments = as.integer(clusterAssignments)
  assert_that(is.na(nClusters) || max(intAssignments) <= nClusters)

  # Determine number of clusters
  if(is.na(nClusters)) {
    if(is.factor(clusterAssignments)) {
      numClus = nlevels(clusterAssignments)
    } else {
      numClus = max(intAssignments)
    }
  } else {
    numClus = nClusters
  }
  assert_that(min(intAssignments) >= 1, max(intAssignments) <= numClus)

  pp = postprobFromAssignments(intAssignments, k=nClusters)

  if(is.null(clusterNames)) {
    clusterNames = make.clusterNames(nClusters)
  }

  clusTrajs = computeCenterClusterTrajectories(data,
                                               assignments=intAssignments,
                                               nClusters=numClus,
                                               fun=center,
                                               id=id,
                                               time=time,
                                               response=response)

  mc = match.call()
  model = new('clModelPartition',
              call = mc,
              center = center,
              clusterTrajectories = clusTrajs,
              postprob = pp,
              name = name,
              clusterNames = clusterNames,
              id = id,
              time = time,
              response = response)
  return(model)
}


setMethod('clusterTrajectories', signature('clModelPartition'), function(object, at, what, ...) {
  if(is.null(at)) {
    clusTrajs = as.data.table(object@clusterTrajectories)
    clusTrajs[, Cluster := factor(Cluster, levels=seq_len(nClusters(object)), labels=clusterNames(object))]
    return(clusTrajs[])
  } else {
    callNextMethod()
  }
})


#. converged ####
setMethod('converged', signature('clModelPartition'), function(object) {
  TRUE
})


# . getName ####
setMethod('getName', signature('clModelPartition'), function(object) object@name)

# . getName0 ####
setMethod('getName0', signature('clModelPartition'), function(object) object@name)


#. postprob ####
setMethod('postprob', signature('clModelPartition'), function(object) {
  pp = object@postprob
  colnames(pp) = clusterNames(object)
  return(pp)
})

