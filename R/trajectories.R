#' @include generics.R latrend.R


# trajectories ####
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



# plotCusterTrajectories ####
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
#' @param trajectories Whether to additionally plot the original trajectories (`TRUE`),
#' or to show the expected interval (standard deviation, standard error, range, or percentile range) of the observations at the respective moment in time.
#'
#' Note that visualizing the expected intervals is currently only supported for time-aligned trajectories, as the interval is computed at each unique moment in time.
#' By default (`FALSE`), no information on the underlying trajectories is shown.
#' @param facet Whether to facet by cluster. This is done by default when `trajectories` is enabled.
#' @param id Id column. Only needed when `trajectories = TRUE`.
setMethod('plotClusterTrajectories', signature('data.frame'), function(object,
  response,
  cluster = 'Cluster',
  time = getOption('latrend.time'),
  center = meanNA,
  trajectories = c(FALSE, 'sd', 'se', '80pct', '90pct', '95pct', 'range'),
  facet = !isFALSE(as.logical(trajectories[1])),
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
    trajectories = trajectories[1],
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
    is.flag(trajectories) || is.character(trajectories),
    length(trajectories) == 1,
    is.flag(facet)
  )

  if (isTRUE(as.logical(trajectories))) {
    # show trajectories
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
  } else if (isFALSE(as.logical(trajectories))) {
    # don't show trajectories
    p = ggplot()
    if (facet) {
      p = p + facet_wrap(cluster)
    }
  } else {
    # ribbon plot
    p = ggplot()
    if (facet) {
      p = p + facet_wrap(cluster)
    }

    if (grepl('^[0-9]{1,2}pct$', trajectories)) {
      # percentile range
      pct = as.integer(substr(trajectories, 0, nchar(trajectories) - 3))
      assert_that(
        pct > 0,
        msg = 'invalid plotClusterTrajectories() argument trajectories = "##pct": ## must be > 0'
      )

      ribbonFun = function(x) quantile(x, probs = .5 + c(-1, 1) * pct / 200, na.rm = TRUE)
      ribbonTitle = sprintf('%dth percentile interval', pct)
    } else {
      trajectories = match.arg(
        trajectories,
        choices = c('sd', 'se', '80pct', '90pct', '95pct', 'range')
      )

      se = function(x) {
        sd(x, na.rm = TRUE) / sqrt(sum(is.finite(x)))
      }

      ribbonFun = switch(trajectories,
        sd = function(x) c(-1, 1) * sd(x, na.rm = TRUE) + mean(x, na.rm = TRUE),
        se = function(x) c(-1, 1) * se(x) + mean(x, na.rm = TRUE),
        range = function(x) range(x, na.rm = TRUE)
      )

      ribbonTitle = switch(trajectories,
        sd = 'SD',
        se = 'standard error',
        range = 'range'
      )
    }

    # compute ribbon
    ribbonData = as.data.table(rawdata)[, as.list(ribbonFun(get(response))), keyby = c(cluster, time)]
    assert_that(ncol(ribbonData) == 4, msg = 'ribbon stat implementation error\nplease report.')
    setnames(ribbonData, c(cluster, time, 'ymin', 'ymax'))

    p = p + geom_ribbon(
      mapping = aes_string(x = time, ymin = 'ymin', ymax = 'ymax'),
      data = ribbonData,
      alpha = .5
    ) + labs(subtitle = sprintf('Range represents %s of local trajectory observations', ribbonTitle))
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


# plotTrajectories ####
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
