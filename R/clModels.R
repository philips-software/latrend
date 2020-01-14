#' @include clModel.R
setOldClass('clModels')

#' @export
#' @title Construct a flat (named) list of clModel objects
#' @description Takes the inputs and generates a named `clModels` object containing a list of the input models. Duplicates are preserved.
#' @param ... `clModel`, `clModels`, or a recursive `list` of `clModel` objects. Arguments may be named.
#' @return A `clModels` object containing all specified `clModel` objects.
#' @examples
#' kml = cluslong(clMethodKML(), testLongData)
#' gmm = cluslong(clMethodGMM(), testLongData)
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
  } else if(is.list(x)) {
    assert_that(all(sapply(x, is.clModel)), msg='object cannot be converted to clModels; not a list of only clModels objects')
  } else if(is.clModel(x)) {
    x = list(x)
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
    newColumns = names(dt)[sapply(dt, uniqueN) > 1]
    dt = dt[, ..newColumns]
  }

  dt[, `.method` := sapply(x, getName0)]
  setcolorder(dt, '.method')
  if(has_name(dt, '.name')) {
    setcolorder(dt, '.name')
  }
  return(dt[])
}


#' @export
#' @family clModel list functions
as.data.frame.clModels = function(x, ...) {
  as.data.table(x, ...) %>% as.data.frame
}

metric.clModels = function(object, name) {
  assert_that(is.clModels(object))
  assert_that(is.character(name))

  modelNames = sapply(object, getName0)
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
  metric.clModels(as.clModels(models), name)
})

#' @export
#' @rdname metric
#' @return For metric(clModels) or metric(list): A data.frame with a metric per column.
setMethod('metric', signature('clModels'), metric.clModels)


#' @export
#' @title Plot one or more internal metrics for all clModels
#' @param models A list of clModels to compute the internal metrics of
#' @inheritParams metric
#' @return `ggplot2` object.
plotMetric = function(models, name) {
  models = as.clModels(models)
  assert_that(is.character(name), name %in% getInternalMetricNames())
  browser()
  df_values = lapply(models, metric, name) %>%
    do.call(rbind, .) %>%
    {reshape2::melt(data=., varnames=c('Model', 'Metric'))}
}


#' @export
#' @title Subsetting a clModels list based on method arguments
#' @param x The `clModels` or list of `clModel` to be subsetted.
#' @param subset Logical expression based on the `clModel` method arguments, indicating which `clModel` objects to keep.
#' @return A `clModels` list with the subset of `clModel` objects.
#' @examples
#' kml1 = cluslong(clMethodKML(nClusters=1), testLongData)
#' kml2 = cluslong(clMethodKML(nClusters=2), testLongData)
#' kml3 = cluslong(clMethodKML(nClusters=3), testLongData)
#' gmm = cluslong(clMethodGMM(), testLongData)
#' models = clModels(kml1, kml2, kml3, gmm)
#'
#' subset(models, nClusters > 1 & method == 'kml')
#' @family clModel list functions
subset.clModels = function(x, subset) {
  x = as.clModels(x)

  if(missing(subset)) {
    return(x)
  }

  subsetCall = match.call()$subset
  dfsub = as.data.table(x) %>%
    .[, .ROW_INDEX := .I] %>%
    base::subset(subset=eval(subsetCall))

  x[dfsub$.ROW_INDEX]
}

#' @export
#' @title Print clModels list concisely
#' @param summary Whether to print the complete summary per model. This may be slow for long lists!
#' @family clModel list functions
print.clModels = function(x, summary=FALSE) {
  if(summary) {
    as.list(x) %>% print
  } else {
    cat(sprintf('List of %d clModels with\n', length(x)))
    print(as.data.table.clModels(x))
  }
}