#' @include modelApprox.R
setClass(
  'lcModelStratify',
  representation(
    clusterTrajectories = 'data.table',
    postprob = 'matrix',
    name = 'character'
  ),
  contains = 'lcApproxModel'
)

.lcModelStratify = function(...) new('lcModelStratify', ...)

#' @rdname interface-custom
#' @inheritParams clusterTrajectories
setMethod('clusterTrajectories', signature('lcModelStratify'),
  function(object, at = time(object), ...) {
  if (length(at) == 0) {
    clusTrajs = copy(object@clusterTrajectories)
    clusTrajs[, Cluster := factor(Cluster,
                                  levels = seq_len(nClusters(object)),
                                  labels = clusterNames(object))]
    return (clusTrajs[])
  } else {
    callNextMethod()
  }
})

#. converged ####
#' @rdname interface-custom
setMethod('converged', signature('lcModelStratify'), function(object, ...) {
  TRUE
})


#. postprob ####
#' @rdname interface-custom
setMethod('postprob', signature('lcModelStratify'), function(object, ...) {
  pp = object@postprob
  colnames(pp) = clusterNames(object)
  return(pp)
})

#. predictPostprob ####
#' @rdname interface-custom
#' @inheritParams predictPostprob
setMethod('predictPostprob', signature('lcModelStratify'), function(object, newdata = NULL, ...) {
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
