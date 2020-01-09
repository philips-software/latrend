#' @include clModel.R

#' @export
is.clModels = function(x) {
  is(x, 'clModels')
}


#' @export
#' @title Convert a list of clModels to a clModels list
as.clModels = function(x) {
  if(is.clModels(x)) {
    x
  } else if(is.list(x)) {
    assert_that(all(sapply(x, is.clModel)), msg='object cannot be converted to clModels; not a list of only clModels objects')
    class(x) = c('clModels', 'list')
    x
  } else if(is.clModel(x)) {
    x = list(x)
    class(x) = c('clModels', 'list')
    x
  } else {
    stop('cannot convert this type of input to clModels')
  }
}


#' @export
as.list.clModels = function(x) {
  class(x) = 'list'
  return(x)
}


#' @export
#' @rdname as.data.frame.clModels
as.data.table.clModels = function(x, excludeShared=TRUE) {
  x = as.clModels(x)

  rawSummaries = lapply(x, getMethod) %>%
    lapply(as.list) %>%
    lapply(function(args) {
      classes = sapply(args, class)
      args[classes %in% c('NULL', 'logical', 'numeric', 'integer', 'character', 'factor', 'formula')]
    })

  atomicSummaries = lapply(rawSummaries, sapply, function(arg) {
    if(is.atomic(arg)) {
      arg
    } else {
      deparse(arg)
    }
  })

  suppressWarnings({
    dt = rbindlist(atomicSummaries, use.names=TRUE, fill=TRUE)
  })

  if(excludeShared && nrow(dt) > 1) {
    newColumns = names(dt)[sapply(dt, uniqueN) > 1]
    dt = dt[, ..newColumns]
  }

  dt[, method := sapply(x, getName0)]
  setcolorder(dt, 'method')
  return(dt[])
}


#' @export
#' @title Generate a data.frame comprising all method arguments of a supported column type
#' @param x A clModels list.
#' @param excludeShared Whether to exclude columns which have the same value across all methods.
as.data.frame.clModels = function(x, excludeShared=TRUE) {
  as.data.table(x) %>% as.data.frame
}


#' @export
#' @title Plot one or more internal metrics for all clModels
#' @param models A list of clModels to compute the internal metrics of
#' @inheritParams metric
plotMetric = function(models, name) {
  x = as.clModels(models)
  assert_that(is.character(name), name %in% getInternalMetricNames())
  df_values = lapply(models, metric, name) %>%
    do.call(rbind, .) %>%
    {reshape2::melt(data=., varnames=c('Model', 'Metric'))}
}


#' @export
#' @title Subsetting a clModels list
subset.clModels = function(x, subset) {

}

#' @export
#' @title Print clModels list concisely
#' @param summary Whether to print the complete summary per model. This may be slow for long lists!
print.clModels = function(x, summary=FALSE) {
  if(summary) {
    as.list(x) %>% print
  } else {
    cat(sprintf('List of %d clModels with\n', length(x)))
    print(as.data.table(x))
  }
}