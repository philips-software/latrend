#' @include model.R
#' @export
#' @title Helper function for ensuring the right fitted() output
#' @description This function is also responsible for checking whether the input data is valid, such that the fitting process can fail early.
#' @return A `vector` if the `clusters` argument is specified, else a `matrix` with the fitted values per cluster per column.
setGeneric('transformFitted', function(pred, model, clusters) standardGeneric('transformFitted'))

setMethod('transformFitted', signature('NULL', 'lcModel'), function(pred, model, clusters) {
  NULL
})

setMethod('transformFitted', signature('matrix', 'lcModel'), function(pred, model, clusters) {
  assert_that(is.matrix(pred),
              ncol(pred) == nClusters(model),
              nrow(pred) == nobs(model))
  colnames(pred) = clusterNames(model)

  if (is.null(clusters)) {
    pred
  } else {
    clusters = make.clusterIndices(model, clusters)
    rowClusters = clusters[genIdRowIndices(model)]
    rowColumns(pred, rowClusters)
  }
})

setMethod('transformFitted', signature('list', 'lcModel'), function(pred, model, clusters) {
  assert_that(length(pred) == nClusters(model))
  newpred = lapply(pred, '[[', 'Fit') %>%
    do.call(cbind, .)
  transformFitted(newpred, model, clusters)
})

setMethod('transformFitted', signature('data.frame', 'lcModel'), function(pred, model, clusters) {
  assert_that(has_name(pred, c('Fit', 'Cluster')))
  newpred = matrix(pred$Fit, ncol = nClusters(model))
  transformFitted(newpred, model, clusters)
})



#' @export
#' @title Helper function that matches the output to the specified newdata
#' @description If Cluster is not provided, the prediction is outputted in long format per cluster,
#' resulting in a longer data.frame than the newdata input
#' @param pred The prediction object
#' @param newdata A `data.frame` containing the input data to predict for
#' @return A data.frame with the predictions, or a list of cluster-specific prediction frames
setGeneric('transformPredict', function(pred, model, newdata)
  standardGeneric('transformPredict'))

setMethod('transformPredict', signature('NULL', 'lcModel'), function(pred, model, newdata) {
  assert_that(is.newdata(newdata),
              nrow(newdata) == 0)
  if (hasName(newdata, 'Cluster')) {
    data.frame(Cluster = factor(levels = seq_len(nClusters(model)),
                                labels = clusterNames(model)),
               Fit = numeric(0))
  } else {
    data.frame(Fit = numeric(0))
  }
})

setMethod('transformPredict', signature('vector', 'lcModel'), function(pred, model, newdata) {
  assert_that(is.newdata(newdata),
              is.null(newdata) || length(pred) == nrow(newdata))
  transformPredict(pred = data.frame(Fit = pred),
                   model = model,
                   newdata = newdata)
})

setMethod('transformPredict', signature('matrix', 'lcModel'), function(pred, model, newdata) {
  # format where multiple cluster-specific predictions are given per newdata entry (per row)
  assert_that(
    is.matrix(pred),
    ncol(pred) == nClusters(model),
    is.newdata(newdata),
    is.null(newdata) || nrow(pred) == nrow(newdata)
  )

  if (hasName(newdata, 'Cluster')) {
    rowClusters = make.clusterIndices(model, newdata$Cluster)
    data.frame(Fit = rowColumns(pred, rowClusters))
  } else {
    data.frame(Fit = as.vector(pred)) %>%
      split(clusterNames(model, factor = TRUE) %>%
              rep(each = nrow(pred)))
  }
})

setMethod('transformPredict', signature('data.frame', 'lcModel'), function(pred, model, newdata) {
  assert_that(is.newdata(newdata),
              !is.null(newdata))
  # generic form, possibly containing more predictions than newdata. These are filtered
  # if the pred object contains the newdata variables. Else, newdata is replicated.
  pred = as.data.table(pred)
  newdata = as.data.table(newdata)

  if (nrow(newdata) == 0) {
    return(as.data.table(pred)[0, ])
  }

  assert_that(hasName(pred, 'Fit'), nrow(pred) > 0)

  mergevars = intersect(names(pred), names(newdata))
  predvars = setdiff(names(pred), names(newdata))

  if (length(mergevars) == 0) {
    if (nrow(pred) == nrow(newdata)) {
      # newdata may have Cluster column, but we assume results are correct since rows match
      if (hasName(newdata, 'Cluster')) {
        newpred = cbind(pred, Cluster = newdata$Cluster)
      } else {
        newpred = pred
      }
    }
    else if (hasName(pred, 'Cluster')) {
      assert_that(nrow(pred) == nrow(newdata) * uniqueN(pred$Cluster), msg = 'cannot merge pred and newdata (no shared columns), and nrow(pred) is not a multiple of nrow(newdata)')
      newpred = pred
    }
    else {
      stop('non-matching rows for pred and newdata, and no shared columns to merge on')
    }
  }
  else if (length(mergevars) == 1 && mergevars == 'Cluster') {
    # can only merge on cluster. order cannot be validated
    # number of observations per cluster must match the number of predictions per cluster
    obsCounts = pred[, .N, keyby = Cluster] %>%
      .[newdata[, .N, keyby = Cluster]]
    assert_that(all(obsCounts[is.finite(N) &
                                is.finite(i.N), N == i.N]), msg = 'number of observations per cluster must match the number of predictions per cluster')

    newpred = pred[Cluster %in% unique(newdata$Cluster)]
  }
  else {
    # attempt to merge pred and newdata to ensure correct filtering of predictions
    newpred = merge(
      newdata,
      pred,
      by = mergevars,
      sort = FALSE,
      allow.cartesian = TRUE
    ) %>%
      subset(select = predvars)
  }


  # only split when newdata does not specify Cluster
  if (hasName(newdata, 'Cluster') || !hasName(newpred, 'Cluster')) {
    newpred
  } else {
    split(newpred, newpred$Cluster)
  }
})
