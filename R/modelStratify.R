#' @include modelApprox.R
.lcModelStratify = setClass(
  'lcModelStratify',
  representation(
    clusterTrajectories = 'data.table',
    postprob = 'matrix',
    name = 'character'
  ),
  contains = 'lcApproxModel'
)

setMethod('clusterTrajectories', signature('lcModelStratify'), function(object, at, what, ...) {
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
setMethod('converged', signature('lcModelStratify'), function(object) {
  TRUE
})


#. postprob ####
setMethod('postprob', signature('lcModelStratify'), function(object) {
  pp = object@postprob
  colnames(pp) = clusterNames(object)
  return(pp)
})

#. predictPostprob ####
setMethod('predictPostprob', signature('lcModelStratify'), function(object, newdata =
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
