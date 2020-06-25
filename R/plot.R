#' @export
setGeneric('plotTrajectories', function(object, ...) standardGeneric('plotTrajectories'))

#' @export
#' @title Plot trajectories from a data.frame
#' @param response Response variable `character` name or a `call`.
#' @param time Time variable name.
#' @param id Id variable name.
#' @param cluster Cluster variable name. If unspecified, trajectories are not grouped. Alternatively, cluster is a vector indicating cluster membership per id.
#' @param facet Whether to facet by cluster
#' @examples
#' data(testLongData)
#' plotTrajectories(testLongData, response = 'Value')
#'
#' plotTrajectories(testLongData, response = quote(exp(Value)))
setMethod('plotTrajectories', signature('data.frame'), function(object,
                                                                response = getOption('cluslong.response'),
                                                                time = getOption('cluslong.time'),
                                                                id = getOption('cluslong.id'),
                                                                cluster = NULL,
                                                                facet = TRUE) {
  if (length(cluster) > 1) {
    assert_that(length(cluster) == uniqueN(object[[id]]))
    object$Cluster = cluster[rleidv(object[[id]])]
    cluster = 'Cluster'
  }
  plotTrajs(object, response, time, id, cluster, facet = facet)
})


plotTrajs = function(data, response, time, id, cluster, facet = TRUE) {
  assert_that(!is.character(response) || has_name(data, response),
              has_name(data, time),
              has_name(data, id),
              is.null(cluster) || has_name(data, cluster),
              is.flag(facet))

  if (!is.null(cluster) && !facet) {
    map = aes_string(x = time, y = response, group = id, cluster = cluster, color = cluster)
  } else {
    map = aes_string(x = time, y = response, group = id, cluster = cluster)
  }

  p = ggplot(data = data, mapping = map) +
    theme(legend.position = 'top') +
    geom_line() +
    labs(title = 'Trajectories')

  if (!is.null(cluster) && facet) {
    p = p + facet_wrap(cluster)
  }
  return(p)
}
