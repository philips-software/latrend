#' @export
setGeneric('plotTrajectories', function(object, ...)
  standardGeneric('plotTrajectories'))

#' @export
#' @title Plot trajectories from a data.frame
#' @param response Response variable name.
#' @param time Time variable name.
#' @param id Id variable name.
#' @param cluster Cluster variable name. If unspecified, trajectories are not grouped. Alternatively, cluster is a vector indicating cluster membership per id.
setMethod('plotTrajectories', signature('data.frame'), function(object,
                                                                response = getOption('cluslong.response'),
                                                                time = getOption('cluslong.time'),
                                                                id = getOption('cluslong.id'),
                                                                cluster = NULL) {
  if (length(cluster) > 1) {
    assert_that(length(cluster) == uniqueN(object[[id]]))
    object$Cluster = cluster[rleidv(object[[id]])]
    cluster = 'Cluster'
  }
  plotTrajs(object, response, time, id, cluster)
})


plotTrajs = function(data, response, time, id, cluster) {
  assert_that(has_name(data, response))
  assert_that(has_name(data, time))
  assert_that(has_name(data, id))
  assert_that(is.null(cluster) || has_name(data, cluster))

  p = ggplot(
    data = data,
    mapping = aes_string(
      x = time,
      y = response,
      group = id,
      cluster = cluster
    )
  ) +
    theme(legend.position = 'top') +
    geom_line() +
    labs(title = 'Trajectories')

  if (!is.null(cluster)) {
    p = p + facet_wrap(cluster)
  }
  return(p)
}
