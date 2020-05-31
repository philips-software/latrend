#' @include clModel.R
setOldClass('clModels')

#' @export
#' @title Construct a flat (named) list of clModel objects
#' @description Takes the inputs and generates a named `clModels` object containing a list of the input models. Duplicates are preserved.
#' @param ... `clModel`, `clModels`, or a recursive `list` of `clModel` objects. Arguments may be named.
#' @return A `clModels` object containing all specified `clModel` objects.
#' @examples
#' kml = cluslong(clMethodKML(), testLongData)
#' gmm = cluslong(clMethodLcmmGMM(), testLongData)
#' clModels(kml, gmm)
#'
#' clModels(defaults=c(kml, gmm))
#' @family clModel list functions
clModels = function(...) {
  list(...) %>%
    unlist %>%
    as.clModels
}

#' @export
`[.clModels` = function(x, i) {
  models = NextMethod()
  as.clModels(models)
}

#' @export
is.clModels = function(x) {
  is(x, 'clModels')
}

#' @export
#' @title Convert a list of clModels to a clModels list
#' @param x An `R` object.
#' @return A `clModels` object.
#' @family clModel list functions
as.clModels = function(x) {
  if(is.clModels(x)) {
    return(x)
  } else if(is.clModel(x)) {
    x = list(x)
  }
  else if(is.list(x)) {
      assert_that(all(vapply(x, is.clModel, FUN.VALUE=FALSE)), msg='object cannot be converted to clModels; not a list of only clModels objects')
  } else if(is.null(x)) {
    x = list()
  } else {
    stop('cannot convert this type of input to clModels')
  }

  class(x) = c('clModels', 'list')
  x
}


#' @export
as.list.clModels = function(x) {
  class(x) = 'list'
  return(x)
}


#' @export
#' @title Generate a data.frame containing the argument values per method per row
#' @inheritParams as.data.frame.clMethod
#' @param x `clModels` or a list of `ClModel`
#' @param excludeShared Whether to exclude columns which have the same value across all methods.
#' @param ... Arguments passed to [as.data.frame.clMethod].
#' @return A `data.frame` or `data.table`.
#' @rdname as.data.frame.clModels
as.data.table.clModels = function(x, excludeShared=FALSE, eval=TRUE, ...) {
  x = as.clModels(x)

  dfs = lapply(x, getMethod) %>%
    lapply(as.data.frame, eval=eval, ...)

  suppressWarnings({
    dt = rbindlist(dfs, use.names=TRUE, fill=TRUE, idcol='.name')
  })

  if(excludeShared && nrow(dt) > 1) {
    newColumns = names(dt)[vapply(dt, uniqueN, FUN.VALUE=0) > 1]
    dt = dt[, ..newColumns]
  }

  dt[, `.method` := vapply(x, getShortName, FUN.VALUE='')]
  if(!has_name(dt, '.name')) {
    dt[, `.name` := character()]
  }

  dataNames = vapply(x, function(model) deparse(getCall(model)$data), FUN.VALUE='')
  if(!excludeShared || uniqueN(dataNames) > 1) {
    dt[, data := dataNames]
  }
  setcolorder(dt, intersect(c('.name', '.method', 'data'), names(dt)))
  return(dt[])
}


#' @export
#' @family clModel list functions
as.data.frame.clModels = function(x, ...) {
  as.data.table(x, ...) %>% as.data.frame
}


#' @export
#' @rdname metric
#' @return A named `numeric` vector containing the computed model metrics.
#' @examples
#' clModel metric example here
setMethod('externalMetric', signature('clModels', 'missing'), function(object, object2, name='AdjustedRand') {
  assert_that(is.character(name), length(name) == 1)

  pairs = combn(seq_along(object), m=2, simplify=FALSE)

  result = lapply(pairs, function(idx) externalMetric(object[[idx[1]]], object[[idx[2]]], name=name) %>% unname())

  m = matrix(NaN, nrow=length(object), ncol=length(object))
  m[do.call(rbind, pairs)] = unlist(result)
  as.dist(t(m), diag=FALSE, upper=FALSE)
})

.externalMetric.clModels = function(object, object2, name) {
  assert_that(is.character(name))

  result = lapply(object, externalMetric, object2=object2, name=name)

  if(length(name) <= 1) {
    do.call(c, result) %>%
      unname()
  } else {
    do.call(rbind, result) %>%
      as.data.frame()
  }
}

#' @export
#' @rdname metric
#' @return A named `numeric` vector containing the computed model metrics.
#' @examples
#' clModel metric example here
setMethod('externalMetric', signature('clModels', 'clModel'), .externalMetric.clModels)

#' @export
#' @rdname metric
#' @return A named `numeric` vector containing the computed model metrics.
#' @examples
#' clModel metric example here
setMethod('externalMetric', signature('list', 'clModel'), function(object, object2, name) {
  assert_that(is.clModels(object))
  .externalMetric.clModels(object, object2, name)
})


.metric.clModels = function(object, name) {
  assert_that(is.clModels(object))
  assert_that(is.character(name))

  modelNames = vapply(object, getShortName, FUN.VALUE='')
  metricValues = lapply(object, function(model) {
    metric(model, name) %>%
      rbind %>%
      data.frame
  })

  dtMetrics = rbindlist(metricValues, idcol='.name')
  dtMetrics[, `.method` := modelNames]

  setcolorder(dtMetrics, '.method')
  if(has_name(dtMetrics, '.name')) {
    setcolorder(dtMetrics, '.name')
  }
  as.data.frame(dtMetrics)
}

#' @export
#' @rdname metric
setMethod('metric', signature('list'), function(object, name) {
  .metric.clModels(as.clModels(object), name)
})

#' @export
#' @rdname metric
#' @return For metric(clModels) or metric(list): A data.frame with a metric per column.
setMethod('metric', signature('clModels'), .metric.clModels)

#' @export
#' @title Plot one or more internal metrics for all clModels
#' @param models A `clModels` or list of `clModel` objects to compute and plot the metrics of.
#' @inheritParams metric
#' @inheritParams subset.clModels
#' @param by The argument name along which methods are plotted.
#' @param group The argument names to use for determining groups of different models. By default, all arguments are included.
#' Specifying group=character() disables grouping.
#' Specifying a single argument for grouping uses that specific column as the grouping column.
#' In all other cases, groupings are represented by a number.
#' @param facet Whether to facet the plot if multiple model groups are identified.
#' @return `ggplot2` object.
#' @examples
#' plotMetric(models, 'BIC', by='nClusters', group='.name')
plotMetric = function(models, name, by='nClusters', subset, group=character()) {
  models = as.clModels(models)
  assert_that(length(models) > 0, msg='need at least 1 clModel to plot')
  assert_that(is.character(name), length(name) >= 1)

  if(!missing(subset)) {
    models = do.call(subset.clModels, list(x=models, subset=substitute(subset)))
  }

  metricNames = paste0('.metric.', name)
  dtMetrics = metric(models, name) %>%
    as.data.table %>%
    .[, -c('.name', '.method')] %>%
    setnames(metricNames)

  dtModels = as.data.table(models)
  assert_that(nrow(dtModels) == nrow(dtMetrics))
  assert_that(is.null(group) || has_name(dtModels, group))

  dtModelMetrics = cbind(dtModels, dtMetrics)
  if(length(group) == 0) {
    dtModelMetrics[, .group := 'All']
  } else {
    dtModelMetrics[, .group := do.call(interaction, subset(dtModelMetrics, select=group))]
  }
  assert_that(has_name(dtModelMetrics, by))

  # Prepare ggplot data; convert to long format to support multiple metrics
  dtgg = melt(dtModelMetrics, id.vars=c(by, '.group'),
              measure.vars=metricNames,
              variable.name='Metric',
              value.name='Value') %>%
    setnames('.group', 'Group')
  levels(dtgg$Metric) = name

  p = ggplot(dtgg, aes_string(x=by, y='Value', group='Group'))

  if(is.numeric(dtModelMetrics[[by]]) || is.logical(dtModelMetrics[[by]])) {
    p = p + geom_line()
  }
  p = p + geom_point()

  if(length(name) == 1) {
    p = p + ylab(name)
  } else {
    p = p + ylab('Value') +
      facet_wrap(~Metric, scales='free_y')
  }

  return(p)
}

#' @export
#' @title Subsetting a clModels list based on method arguments
#' @param x The `clModels` or list of `clModel` to be subsetted.
#' @param subset Logical expression based on the `clModel` method arguments, indicating which `clModel` objects to keep.
#' @param drop Whether to return a `clModel` object if the result is length 1.
#' @return A `clModels` list with the subset of `clModel` objects.
#' @examples
#' kml1 = cluslong(clMethodKML(nClusters=1), testLongData)
#' kml2 = cluslong(clMethodKML(nClusters=2), testLongData)
#' kml3 = cluslong(clMethodKML(nClusters=3), testLongData)
#' gmm = cluslong(clMethodLcmmGMM(), testLongData)
#' models = clModels(kml1, kml2, kml3, gmm)
#'
#' subset(models, nClusters > 1 & .method == 'kml')
#' @family clModel list functions
subset.clModels = function(x, subset, drop=FALSE) {
  x = as.clModels(x)

  if(missing(subset)) {
    return(x)
  }

  subsetCall = match.call()$subset
  dfsub = as.data.table(x) %>%
    .[, .ROW_INDEX := .I] %>%
    base::subset(subset=eval(subsetCall))

  if(drop && nrow(dfsub) == 1) {
    x[[dfsub$.ROW_INDEX]]
  } else {
    x[dfsub$.ROW_INDEX]
  }
}

#' @export
#' @title Print clModels list concisely
#' @param summary Whether to print the complete summary per model. This may be slow for long lists!
#' @family clModel list functions
print.clModels = function(x, summary=FALSE, excludeShared=!getOption('cluslong.printSharedModelArgs')) {
  if(summary) {
    as.list(x) %>% print
  } else {
    cat(sprintf('List of %d clModels with\n', length(x)))
    print(as.data.table.clModels(x, excludeShared=excludeShared))
  }
}