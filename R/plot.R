# plotTrajectories ####
#' @export
#' @rdname plotTrajectories
#' @title Plot trajectories
#' @inheritParams trajectories
#' @inheritParams transformLatrendData
#' @param response Response variable `character` name or a `call`.
#' @param cluster Cluster variable name. If unspecified, trajectories are not grouped. Alternatively, cluster is a vector indicating cluster membership per id.
#' @param facet Whether to facet by cluster.
#' @param ... Additional arguments.
#' @examples
#' data(latrendData)
#' plotTrajectories(latrendData, response = 'Y')
#'
#' plotTrajectories(latrendData, response = quote(exp(Y)))
setMethod('plotTrajectories', signature('data.frame'), function(object,
                                                                response,
                                                                time = getOption('latrend.time'),
                                                                id = getOption('latrend.id'),
                                                                cluster = NULL,
                                                                facet = TRUE,
                                                                ...) {
  if (length(cluster) > 1) {
    assert_that(length(cluster) == uniqueN(object[[id]]))
    object$Cluster = cluster[rleidv(object[[id]])]
    cluster = 'Cluster'
  }
  .plotTrajs(object, response, time, id, cluster, facet = facet)
})


.plotTrajs = function(data, response, time, id, cluster, facet = TRUE) {
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

# plotClusterTrajectories ####
#' @export
#' @rdname plotClusterTrajectories
#' @title Plot cluster trajectories
#' @inheritParams transformLatrendData
#' @inheritParams clusterTrajectories
#' @inheritParams predict.lcModel
#' @param object The (cluster) trajectory data.
#' @param cluster The cluster assignment column
#' @param center A function for aggregating multiple points at the same point in time
#' @param showTrajs Whether to plot the original data in addition to the cluster (i.e., center) trajectories
#' @param id Id column. Only needed when `showTrajs = TRUE`.
#' @param ... Additional arguments.
setMethod('plotClusterTrajectories', signature('data.frame'), function(object,
    response,
    cluster = 'Cluster',
    time = getOption('latrend.time'),
    center = meanNA,
    showTrajs = FALSE,
    id = getOption('latrend.id'),
    ...
  ) {
  assert_that(has_name(object, cluster),
    has_name(object, response),
    is.function(center),
    is.flag(showTrajs))

  cdata = as.data.table(object) %>%
    .[, .(Value = center(get(response))), keyby=c(cluster, time)] %>%
    setnames('Value', response)

  .plotClusterTrajs(cdata,
    response = response,
    time = time,
    cluster = cluster,
    showTrajs = showTrajs,
    id = id,
    rawdata = object)
})


.plotClusterTrajs = function(data, response, time, cluster = 'Cluster', showTrajs = FALSE, id, rawdata = NULL) {
  assert_that(is.data.frame(data),
    has_name(data, response),
    has_name(data, time),
    has_name(data, cluster),
    is.flag(showTrajs))

  if (showTrajs) {
    assert_that(is.data.frame(rawdata),
      has_name(rawdata, id),
      has_name(rawdata, time),
      has_name(rawdata, response))

    rawdata = subset(rawdata, select = c(id, time, response))
  }

  if (is.factor(data[[cluster]])) {
    nClus = nlevels(data[[cluster]])
  } else {
    nClus = uniqueN(data[[cluster]])
  }

  p = ggplot(
    data = data,
    mapping = aes_string(
      x = time,
      y = response)
  )

  if (showTrajs) {
    p = p + geom_line(data = rawdata, mapping = aes_string(group = id), size = .1, color = 'black')
  }

  p = p + geom_line(aes_string(color = cluster)) +
    labs(title = 'Cluster trajectories')

  return(p)
}
