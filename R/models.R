#' @include model.R

setOldClass('lcModels')

#' @export
#' @aliases lcModels-class
#' @title Construct a flat (named) list of lcModel objects
#' @description The `lcModels` `S3` class represents a `list` of `lcModel` objects.
#' This makes it easier to work with a set of models in a more structured manner.
#'
#' The `lcModels()` function takes the inputs and generates a named `lcModels` object containing a list of the input models. Duplicates are preserved.
#' @param ... `lcModel`, `lcModels`, or a recursive `list` of `lcModel` objects. Arguments may be named.
#' @return A `lcModels` object containing all specified `lcModel` objects.
#' @examples
#' data(latrendData)
#' kml <- latrend(lcMethodKML("Y", id = "Id", time = "Time"), latrendData)
#' gmm <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
#'    id = "Id", time = "Time"), latrendData)
#' lcModels(kml, gmm)
#'
#' lcModels(defaults = c(kml, gmm))
#' @family lcModel list functions
lcModels = function(...) {
  list(...) %>%
    unlist() %>%
    as.lcModels()
}

#' @export
`[.lcModels` = function(x, i) {
  models = NextMethod()
  as.lcModels(models)
}

#' @export
#' @rdname is
is.lcModels = function(x) {
  is(x, 'lcModels')
}

#' @export
#' @title Convert a list of lcModels to a lcModels list
#' @param x A `list` of `lcModel` objects, an `lcModels` object, or `NULL`.
#' @return A `lcModels` object.
#' @seealso lcModels
#' @family lcModel list functions
as.lcModels = function(x) {
  if (missing(x) || is.null(x)) {
    x = list()
  }
  else if (is.lcModels(x)) {
    return(x)
  }
  else if (is.lcModel(x)) {
    x = list(x)
  }
  else if (is.list(x)) {
    assert_that(all(vapply(x, is.lcModel, FUN.VALUE = FALSE)),
      msg = 'object cannot be converted to lcModels; not a list of only lcModels objects')
  }
  else {
    stop('cannot convert this type of input to lcModels')
  }

  class(x) = c('lcModels', 'list')
  x
}


#' @export
as.list.lcModels = function(x, ...) {
  assert_that(is.lcModels(x) || is.list(x) || is.null(x))
  class(x) = 'list'
  return(x)
}


#' @export
#' @title Generate a data.frame containing the argument values per method per row
#' @inheritParams as.data.frame.lcMethod
#' @param x `lcModels` or a list of `lcModel`
#' @param excludeShared Whether to exclude columns which have the same value across all methods.
#' @param ... Arguments passed to [as.data.frame.lcMethod].
#' @return A `data.frame`.
as.data.frame.lcModels = function(x, ...,
                                  excludeShared = FALSE,
                                  eval = TRUE) {
  x = as.lcModels(x)

  dfs = lapply(x, getLcMethod) %>%
    lapply(as.data.frame, eval = eval, ...)

  suppressWarnings({
    dt = rbindlist(dfs,
                   use.names = TRUE,
                   fill = TRUE,
                   idcol = '.name')
  })

  if (isTRUE(excludeShared) && nrow(dt) > 1) {
    newColumns = names(dt)[vapply(dt, uniqueN, FUN.VALUE = 0) > 1]
    dt = subset(dt, select = newColumns)
  }

  dt[, `.method` := vapply(x, getShortName, FUN.VALUE = '')]
  if (!has_name(dt, '.name')) {
    dt[, `.name` := character()]
  }

  dataNames = vapply(x, function(model)
    deparse(getCall(model)$data), FUN.VALUE = '')
  if (!excludeShared || uniqueN(dataNames) > 1) {
    dt[, data := dataNames]
  }
  setcolorder(dt, intersect(c('.name', '.method', 'data'), names(dt)))
  as.data.frame(dt)
}

# externalMetric ####
.externalMetric.lcModels = function(object, object2, name, drop = TRUE) {
  assert_that(is.character(name),
    is.flag(drop))

  if (length(object) == 0) {
    if (drop) {
      numeric()
    } else {
      matrix(nrow=0, ncol=length(name)) %>%
        set_colnames(name) %>%
        as.data.frame()
    }
  }
  else {
    metMat = lapply(object, externalMetric, object2 = object2, name = name) %>%
      do.call(rbind, .) %>%
      set_colnames(name)

    if(drop && ncol(metMat) == 1) {
      as.numeric(metMat)
    } else {
      as.data.frame(metMat)
    }
  }
}

.externalMetricDist.lcModels = function(object, name) {
  assert_that(length(name) > 0, msg = 'no external metric names provided')
  assert_that(
    is.character(name),
    length(name) == 1
  )

  pairs = combn(seq_along(object), m = 2, simplify = FALSE)

  result = lapply(pairs, function(idx)
    externalMetric(object[[idx[1]]], object[[idx[2]]], name = name) %>% unname())

  m = matrix(NaN, nrow = length(object), ncol = length(object))
  m[do.call(rbind, pairs)] = unlist(result)
  as.dist(t(m), diag = FALSE, upper = FALSE)
}

#' @export
#' @importFrom stats as.dist
#' @rdname externalMetric
#' @aliases externalMetric,lcModels,missing-method
#' @return For `externalMetric(lcModels)`: A distance matrix of class [dist] representing
#' the pairwise comparisons.
setMethod('externalMetric', signature('lcModels', 'missing'),
  function(object, object2, name = 'adjustedRand') {
    .externalMetricDist.lcModels(object, name = name)
})

#' @export
#' @importFrom stats as.dist
#' @rdname externalMetric
#' @aliases externalMetric,lcModels,character-method
#' @return For `externalMetric(lcModels, name)`: A distance matrix of class [dist] representing
#' the pairwise comparisons.
setMethod('externalMetric', signature('lcModels', 'character'),
  function(object, object2 = 'adjustedRand') {
    .externalMetricDist.lcModels(object, name = object2)
})


#' @export
#' @rdname externalMetric
#' @aliases externalMetric,lcModels,lcModels-method
#' @return For `externalMetric(lcModels, lcModel)`: A named `numeric` vector or `data.frame`
#' containing the computed model metrics.
setMethod('externalMetric', signature('lcModels', 'lcModel'), .externalMetric.lcModels)


#' @export
#' @rdname externalMetric
#' @aliases externalMetric,list,lcModel-method
#' @inheritParams metric
#' @return For `externalMetric(list, lcModel)`: A named `numeric` vector or `data.frame`
#' containing the computed model metrics.
setMethod('externalMetric', signature('list', 'lcModel'),
  function(object, object2, name, drop = TRUE) {
    models = as.lcModels(object)
    .externalMetric.lcModels(models, object2, name, drop = drop)
})


# metric ####
.metric.lcModels = function(object, name, drop = TRUE) {
  assert_that(length(name) > 0, msg = 'no metric names provided')
  assert_that(is.lcModels(object),
    is.character(name),
    is.flag(drop))

  if (length(object) == 0) {
    if (drop) {
      numeric()
    } else {
      matrix(nrow = 0, ncol = length(name)) %>%
        set_colnames(name) %>%
        as.data.frame()
    }
  }
  else {
    metMat = lapply(object, metric, name = name) %>%
      do.call(rbind, .) %>%
      set_colnames(name)

    if(drop && ncol(metMat) == 1) {
      as.numeric(metMat)
    } else {
      as.data.frame(metMat)
    }
  }
}

#' @export
#' @rdname metric
#' @param drop Whether to return a `numeric vector` instead of a `data.frame`
#' in case of a single metric.
#' @return For `metric(list)`: A `data.frame` with a metric per column.
setMethod('metric', signature('list'), function(object, name, drop = TRUE) {
  .metric.lcModels(as.lcModels(object), name, drop = drop)
})

#' @export
#' @rdname metric
#' @return For `metric(lcModels)`: A `data.frame` with a metric per column.
setMethod('metric', signature('lcModels'), .metric.lcModels)

#' @export
#' @title Select the lcModel with the lowest metric value
#' @param x The `lcModels` object
#' @param name The name of the internal metric.
#' @param ... Additional arguments.
#' @return The lcModel with the lowest metric value
#' @examples
#' data(latrendData)
#' baseMethod <- lcMethodKML(response = "Y", id = "Id", time = "Time")
#' kml1 <- latrend(baseMethod, nClusters = 1, latrendData)
#' kml2 <- latrend(baseMethod, nClusters = 2, latrendData)
#' kml3 <- latrend(baseMethod, nClusters = 3, latrendData)
#' models <- lcModels(kml1, kml2, kml3)
#' min(models, 'WRSS')
#' @seealso [max.lcModels] [externalMetric]
min.lcModels = function(x, name, ...) {
  x = as.lcModels(x)
  values = metric(x, name)
  bestIdx = which.min(values)
  if (length(bestIdx) == 0) {
    return(as.lcModels(NULL))
  } else {
    x[[bestIdx]]
  }
}

#' @export
#' @title Select the lcModel with the highest metric value
#' @param x The `lcModels` object.
#' @param ... Additional arguments.
#' @param name The name of the internal metric.
#' @return The lcModel with the highest metric value
#' @examples
#' data(latrendData)
#' baseMethod <- lcMethodKML(response = "Y", id = "Id", time = "Time")
#' kml1 <- latrend(baseMethod, nClusters = 1, latrendData)
#' kml2 <- latrend(baseMethod, nClusters = 2, latrendData)
#' kml3 <- latrend(baseMethod, nClusters = 3, latrendData)
#' models <- lcModels(kml1, kml2, kml3)
#' max(models, 'WRSS')
#' @seealso [min.lcModels] [externalMetric]
max.lcModels = function(x, name, ...) {
  x = as.lcModels(x)
  values = metric(x, name)
  bestIdx = which.max(values)
  if (length(bestIdx) == 0) {
    return(as.lcModels(NULL))
  } else {
    x[[bestIdx]]
  }
}


#' @export
#' @title Plot one or more internal metrics for all lcModels
#' @param models A `lcModels` or list of `lcModel` objects to compute and plot the metrics of.
#' @inheritParams metric
#' @inheritParams subset.lcModels
#' @param by The argument name along which methods are plotted.
#' @param group The argument names to use for determining groups of different models. By default,
#' all arguments are included.
#' Specifying `group = character()` disables grouping.
#' Specifying a single argument for grouping uses that specific column as the grouping column.
#' In all other cases, groupings are represented by a number.
#' @return `ggplot2` object.
#' @examples
#' data(latrendData)
#' baseMethod <- lcMethodKML(response = "Y", id = "Id", time = "Time")
#' methods <- lcMethods(baseMethod, nClusters = 1:6)
#' models <- latrendBatch(methods, latrendData)
#' plotMetric(models, c("BIC", "WRSS"))
plotMetric = function(models,
                      name,
                      by = 'nClusters',
                      subset,
                      group = character()) {
  models = as.lcModels(models)
  assert_that(length(models) > 0, msg = 'need at least 1 lcModel to plot')
  assert_that(is.character(name), length(name) >= 1)

  if (!missing(subset)) {
    models = do.call(subset.lcModels, list(x = models, subset = substitute(subset)))
  }

  metricNames = paste0('.metric.', name)
  dtMetrics = metric(models, name, drop = FALSE) %>%
    set_colnames(metricNames) %>%
    as.data.table()

  dtModels = as.data.frame(models) %>%
    as.data.table()

  assert_that(
    nrow(dtModels) == nrow(dtMetrics),
    is.null(group) || has_name(dtModels, group)
  )

  dtModelMetrics = cbind(dtModels, dtMetrics)
  if (length(group) == 0) {
    dtModelMetrics[, .group := 'All']
  } else {
    dtModelMetrics[, .group := do.call(interaction, base::subset(dtModelMetrics, select = group))]
  }
  assert_that(has_name(dtModelMetrics, by))

  # Prepare ggplot data; convert to long format to support multiple metrics
  dtgg = melt(
    dtModelMetrics,
    id.vars = c(by, '.group'),
    measure.vars = metricNames,
    variable.name = 'Metric',
    value.name = 'Value'
  ) %>%
    setnames('.group', 'Group')
  levels(dtgg$Metric) = name

  p = ggplot(dtgg, aes_string(x = by, y = 'Value', group = 'Group'))

  if (is.numeric(dtModelMetrics[[by]]) || is.logical(dtModelMetrics[[by]])) {
    p = p + geom_line()
  }
  p = p + geom_point()

  if (by == 'nClusters') {
    p = p + scale_x_continuous(breaks = seq(1, max(dtModelMetrics[[by]])), minor_breaks = NULL)
  }

  if (length(name) == 1) {
    p = p + ylab(name)
  } else {
    p = p + ylab('Value') +
      facet_wrap( ~ Metric, scales = 'free_y')
  }

  return(p)
}

#' @export
#' @title Subsetting a lcModels list based on method arguments
#' @param x The `lcModels` or list of `lcModel` to be subsetted.
#' @param subset Logical expression based on the `lcModel` method arguments, indicating
#' which `lcModel` objects to keep.
#' @param ... Not used.
#' @param drop Whether to return a `lcModel` object if the result is length 1.
#' @return A `lcModels` list with the subset of `lcModel` objects.
#' @examples
#' data(latrendData)
#' mKML <- lcMethodKML(response = "Y", id = "Id", time = "Time")
#' kml1 <- latrend(mKML, nClusters = 1, latrendData)
#' kml2 <- latrend(mKML, nClusters = 2, latrendData)
#' kml3 <- latrend(mKML, nClusters = 3, latrendData)
#' gmm <- latrend(lcMethodLcmmGMM(fixed = Y ~ Time, mixture = ~ Time,
#'    id = "Id", time = "Time"), latrendData)
#' models <- lcModels(kml1, kml2, kml3, gmm)
#'
#' subset(models, nClusters > 1 & .method == 'kml')
#' @family lcModel list functions
subset.lcModels = function(x, subset, drop = FALSE, ...) {
  x = as.lcModels(x)

  if (missing(subset)) {
    return(x)
  }

  subsetCall = match.call()$subset
  dfsub = as.data.frame(x) %>%
    as.data.table() %>%
    .[, .ROW_INDEX := .I] %>%
    base::subset(subset = eval(subsetCall))

  if (isTRUE(drop) && nrow(dfsub) == 1) {
    x[[dfsub$.ROW_INDEX]]
  } else {
    x[dfsub$.ROW_INDEX]
  }
}

#' @export
#' @title Print lcModels list concisely
#' @param x The `lcModels` object.
#' @param ... Not used.
#' @param summary Whether to print the complete summary per model. This may be slow for long lists!
#' @param excludeShared Whether to exclude model arguments which are identical across all models.
#' @family lcModel list functions
print.lcModels = function(x,
                          ...,
                          summary = FALSE,
                          excludeShared = !getOption('latrend.printSharedModelArgs')) {
  if (isTRUE(summary)) {
    as.list(x) %>% print()
  } else {
    cat(sprintf('List of %d lcModels with\n', length(x)))
    print(as.data.frame.lcModels(x, excludeShared = excludeShared))
  }
}
