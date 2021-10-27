#' @include modelApprox.R
setClass('lcModelStratify', contains = 'lcModelPartition')

.lcModelStratify = function(...) new('lcModelStratify', ...)

#. converged ####
#' @rdname interface-custom
setMethod('converged', 'lcModelStratify', function(object, ...) {
  TRUE
})


#. predictPostprob ####
#' @rdname interface-custom
#' @inheritParams predictPostprob
setMethod('predictPostprob', 'lcModelStratify', function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(postprob(object))
  }

  assignments = .stratifyTrajectories(
    method[['stratify', eval = FALSE]],
    data = newdata,
    id = idVariable(method)
  ) %>%
    as.integer()

  pp = postprobFromAssignments(assignments, nClusters(object))
  colnames(pp) = clusterNames(object)
  return(pp)
})
