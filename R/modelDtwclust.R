#' @include model.R
setClass('lcModelDtwclust', contains = 'lcModel')

#. converged ####
#' @rdname interface-dtwclust
setMethod('converged', 'lcModelDtwclust', function(object, ...) {
  object@model@converged
})

#. postprob ####
#' @rdname interface-dtwclust
setMethod('postprob', 'lcModelDtwclust', function(object, ...) {
  if('fcluster' %in% slotNames(object@model)) {
    pp = object@model@fcluster
  } else {
    pp = postprobFromAssignments(object@model@cluster, object@model@k)
  }

  colnames(pp) = clusterNames(object)
  return(pp)
})

#. predictForCluster ####
#' @rdname interface-dtwclust
#' @inheritParams predictForCluster
setMethod('predictForCluster', 'lcModelDtwclust', function(object, newdata, cluster, what = 'mu', ...) {
  assert_that(cluster %in% clusterNames(object))

  k = match(cluster, clusterNames(object))
  centroid = object@model@centroids[[k]]

  if('series_id' %in% attributes(object@model@centroids)) {
    centroidId = ids(object)[attr(object@model@centroids, 'series_id')[k]]
    times = model.data(object) %>%
      as.data.table() %>%
      .[get(idVariable(object)) == centroidId] %>%
      .[, get(timeVariable(object))]
  } else {
    times = time(object)
  }

  assert_that(length(times) == length(centroid))
  assert_that(all(newdata[[timeVariable(object)]] %in% times), msg='predict not supported for unobserved moments in time')

  centroid[match(newdata[[timeVariable(object)]], times)]
})
