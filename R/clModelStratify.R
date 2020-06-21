#' @include clApproxModel.R
.clModelStratify = setClass(
  'clModelStratify',
  representation(
    clusterTrajectories = 'data.table',
    postprob = 'matrix',
    name = 'character'
  ),
  contains = 'clApproxModel'
)

setMethod('clusterTrajectories', signature('clModelStratify'), function(object, at, what, ...) {
  if (is.null(at)) {
    clusTrajs = copy(object@clusterTrajectories)
    clusTrajs[, Cluster := factor(Cluster,
                                  levels = seq_len(nClusters(object)),
                                  labels = clusterNames(object))]
    return(clusTrajs[])
  } else {
    callNextMethod()
  }
})

#. converged ####
setMethod('converged', signature('clModelStratify'), function(object) {
  TRUE
})


#. postprob ####
setMethod('postprob', signature('clModelStratify'), function(object) {
  pp = object@postprob
  colnames(pp) = clusterNames(object)
  return(pp)
})

#. predictPostprob ####
setMethod('predictPostprob', signature('clModelStratify'), function(object, newdata =
                                                                      NULL, ...) {
  if (is.null(newdata)) {
    return(postprob(object))
  }

  assignments = stratifyTrajectories(method[['stratify', eval = FALSE]],
                                     data = newdata,
                                     id = idVariable(method)) %>%
    as.integer()

  pp = postprobFromAssignments(assignments, nClusters(object))
  colnames(pp) = clusterNames(object)
  return(pp)
})
