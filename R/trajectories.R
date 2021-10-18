#' @include generics.R latrend.R

# Trajectories ####
#' @rdname trajectories
#' @aliases trajectories,data.frame-method
setMethod('trajectories', signature('data.frame'), function(object, id, time, response, ...) {
  assert_that(
    has_name(object, id),
    has_name(object, time),
    is.numeric(object[[id]]) || is.factor(object[[id]]) || is.character(object[[id]]),
    noNA(object[[id]]),
    noNA(object[[time]])
  )

  object
})


#' @rdname trajectories
#' @aliases trajectories,matrix-method
setMethod('trajectories', signature('matrix'), function(object, id, time, response, ...) {
  data = meltRepeatedMeasures(object, id = id, time = time, response = response)
  trajectories(data, id = id, time = time, response = response, ...)
})


#' @rdname trajectories
#' @aliases trajectories,call-method
#' @param envir The `environment` used to evaluate the data object in (e.g., in case `object` is of type `call`).
setMethod('trajectories', signature('call'), function(object, ..., envir) {
  data = eval(object, envir = envir)
  trajectories(data, ..., envir = envir)
})


# Plot cluster trajectories ####
#' @export
#' @name plotClusterTrajectories
#' @rdname plotClusterTrajectories
#' @aliases plotClusterTrajectories,data.frame-method
#' @title Plot cluster trajectories
#' @inheritParams trajectories
#' @inheritParams clusterTrajectories
#' @inheritParams predict.lcModel
#' @param object The (cluster) trajectory data.
#' @param cluster The cluster assignment column
#' @param center A function for aggregating multiple points at the same point in time
#' @param trajectories Whether to plot the original data in addition to the cluster (i.e., center) trajectories
#' @param facet Whether to facet by cluster. This is done by default when `trajectories` is enabled.
#' @param id Id column. Only needed when `trajectories = TRUE`.
setMethod('plotClusterTrajectories', signature('data.frame'), function(object,
  response,
  cluster = 'Cluster',
  time = getOption('latrend.time'),
  center = meanNA,
  trajectories = FALSE,
  facet = isTRUE(trajectories),
  id = getOption('latrend.id'),
  ...
) {
  assert_that(
    has_name(object, cluster),
    has_name(object, response),
    is.function(center)
  )

  clusTrajData = as.data.table(object) %>%
    .[, .(Value = center(get(response))), keyby = c(cluster, time)] %>%
    setnames('Value', response)

  .plotClusterTrajs(
    clusTrajData,
    response = response,
    time = time,
    cluster = cluster,
    trajectories = trajectories,
    facet = facet,
    id = id,
    rawdata = object,
    ...
  )
})


.plotClusterTrajs = function(
  data,
  response,
  time,
  cluster = 'Cluster',
  trajectories = FALSE,
  facet = FALSE,
  id,
  rawdata = NULL,
  ...
) {
  assert_that(
    is.data.frame(data),
    has_name(data, response),
    has_name(data, time),
    has_name(data, cluster),
    is.flag(trajectories),
    is.flag(facet)
  )

  if (isTRUE(trajectories)) {
    assert_that(
      is.data.frame(rawdata),
      nrow(rawdata) > 0,
      !missing(id)
    )

    p = plotTrajectories(
      rawdata,
      response = response,
      time = time,
      id = id,
      facet = facet,
      cluster = cluster,
      ...
    )
  } else {
    p = ggplot()
    if (facet) {
      p = p + facet_wrap(cluster)
    }
  }

  if (facet) {
    p = p + guides(color = 'none')
  }

  # add cluster trajectories to plot
  p = p + geom_line(
    mapping = aes_string(x = time, y = response, color = cluster),
    data = data,
    ...
  ) + labs(title = 'Cluster trajectories')

  return(p)
}


# Plot trajectories ####
#' @export
#' @name plotTrajectories
#' @rdname plotTrajectories
#' @aliases plotTrajectories,data.frame-method
#' @inheritParams trajectories
#' @param response Response variable `character` name or a `call`.
#' @param cluster Cluster variable name. If unspecified, trajectories are not grouped. Alternatively, cluster is a vector indicating cluster membership per id.
#' @param facet Whether to facet by cluster.
#' @seealso [trajectories] [plotFittedTrajectories] [plotClusterTrajectories]
#' @examples
#' data(latrendData)
#' plotTrajectories(latrendData, response = "Y", id = "Id", time = "Time")
#'
#' plotTrajectories(latrendData, response = quote(exp(Y)), id = "Id", time = "Time")
#'
#' plotTrajectories(latrendData, response = "Y", id = "Id", time = "Time", cluster = "Class")
#'
#' # compute cluster membership based on the mean being below 0
#' assignments = aggregate(Y ~ Id, latrendData, mean)$Y < 0
#' plotTrajectories(latrendData,
#'   response = "Y", id = "Id", time = "Time", cluster = assignments)
setMethod('plotTrajectories', signature('data.frame'),
  function(
    object,
    response,
    time = getOption('latrend.time'),
    id = getOption('latrend.id'),
    cluster = NULL,
    facet = TRUE,
    ...
  ) {

  assert_that(
    !is.character(response) || has_name(object, response),
    has_name(object, time),
    has_name(object, id),
    length(cluster) != 1 || (is.character(cluster) && has_name(object, cluster)),
    is.flag(facet)
  )

  if (length(cluster) > 1) {
    assert_that(length(cluster) == uniqueN(object[[id]]))
    object$Cluster = cluster[rleidv(object[[id]])]
    cluster = 'Cluster'
  }

  if (!is.null(cluster) && !facet) {
    map = aes_string(x = time, y = response, group = id, color = cluster)
  } else {
    map = aes_string(x = time, y = response, group = id)
  }

  p = ggplot() +
    theme(legend.position = 'top') +
    geom_line(mapping = map, data = object) +
    labs(title = 'Trajectories')

  if (!is.null(cluster) && facet) {
    p = p + facet_wrap(cluster)
  }
  return(p)
})


#' @export
#' @rdname plotTrajectories
#' @aliases plotTrajectories,ANY-method
setMethod('plotTrajectories', signature('ANY'), function(object, ...) {
  data = trajectories(object, ...)
  plotTrajectories(data, ...)
})
