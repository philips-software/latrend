#' @include model.R

#' @export
#' @rdname transformFitted
#' @title Helper function for ensuring the right fitted() output
#' @description A helper function for implementing the [fitted.lcModel()][fitted.lcModel] method as part of your own `lcModel` class. This function has no use outside of `lcModel` class implementations.
#' The function ensures that the output format is correct for the given the input (see the Value section).
#' It also checks whether the input data is valid.
#'
#' The prediction ordering depends on the original ordering of the observations in the data that was used for fitting the `lcModel`.
#'
#' By default, `transformFitted` accepts one of the following inputs:
#' \describe{
#'  \item{`data.frame`}{A `data.frame` in long format providing a cluster-specific prediction for each observation per row, with column names `"Fit"` and `"Cluster"`. This `data.frame` therefore has `nObs(object) * nClusters(object)` rows.}
#'  \item{`matrix`}{An N-by-K `matrix` where each row provides the cluster-specific predictions for the respective observation. Here, `N = nObs(object)` and `K = nClusters(object)`.}
#'  \item{`list`}{A `list` of cluster-specific prediction `vector`s. Each prediction vector should be of length `nObs(object)`. The overall (named) list of cluster-specific prediction vectors is of length `nClusters(object)`.}
#' }
#'
#' Users can implement support for other prediction formats by defining the `transformFitted` method with other signatures.
#'
#' @section Example implementation:
#' A typical implementation of `fitted.lcModel` for your own `lcModel` class would have the following format:
#' \preformatted{
#' fitted.lcModelExample <- function(object, clusters = trajectoryAssignments(object)) {
#'   # computations of the fitted values per cluster here
#'   predictionMatrix <- CODE_HERE
#'   transformFitted(pred = predictionMatrix, model = object, clusters = clusters)
#' }
#' }
#'
#' For a complete and runnable example, see the custom models vignette accessible via \code{vignette("custom", package = "latrend")}.
#'
#' @usage transformFitted(pred, model, clusters)
#' @param pred The cluster-specific predictions for each observation
#' @param model The `lcModel` by which the prediction was made.
#' @param clusters The trajectory cluster assignment per observation. Optional.
#' @return If the `clusters` argument was specified, a `vector` of fitted values conditional on the given cluster assignment. Else, a `matrix` with the fitted values per cluster per column.
setGeneric('transformFitted', function(pred, model, clusters) standardGeneric('transformFitted'))

#' @rdname transformFitted
#' @aliases transformFitted,NULL,lcModel-method
setMethod('transformFitted', signature('NULL', 'lcModel'), function(pred, model, clusters) {
  NULL
})

#' @rdname transformFitted
#' @aliases transformFitted,matrix,lcModel-method
setMethod('transformFitted', signature('matrix', 'lcModel'), function(pred, model, clusters) {
  assert_that(is.matrix(pred),
              ncol(pred) == nClusters(model),
              nrow(pred) == nobs(model))
  colnames(pred) = clusterNames(model)

  if (is.null(clusters)) {
    pred
  } else {
    clusters = make.clusterIndices(model, clusters)
    rowClusters = clusters[make.idRowIndices(model)]
    rowColumns(pred, rowClusters)
  }
})

#' @rdname transformFitted
#' @aliases transformFitted,list,lcModel-method
setMethod('transformFitted', signature('list', 'lcModel'), function(pred, model, clusters) {
  assert_that(length(pred) == nClusters(model))
  newpred = lapply(pred, '[[', 'Fit') %>%
    do.call(cbind, .)
  transformFitted(newpred, model, clusters)
})

#' @rdname transformFitted
#' @aliases transformFitted,data.frame,lcModel-method
setMethod('transformFitted', signature('data.frame', 'lcModel'), function(pred, model, clusters) {
  assert_that(has_name(pred, c('Fit', 'Cluster')))
  newpred = matrix(pred$Fit, ncol = nClusters(model))
  transformFitted(newpred, model, clusters)
})



#' @export
#' @rdname transformPredict
#' @usage transformPredict(pred, model, newdata)
#' @title Helper function that matches the output to the specified newdata
#' @description If Cluster is not provided, the prediction is outputted in long format per cluster,
#' resulting in a longer data.frame than the newdata input
#' @param pred The prediction object
#' @param model The model for which the prediction is made.
#' @param newdata A `data.frame` containing the input data to predict for.
#' @return A data.frame with the predictions, or a list of cluster-specific prediction frames
setGeneric('transformPredict', function(pred, model, newdata) standardGeneric('transformPredict'))

#' @rdname transformPredict
#' @aliases transformPredict,NULL,lcModel-method
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

#' @rdname transformPredict
#' @aliases transformPredict,vector,lcModel-method
setMethod('transformPredict', signature('vector', 'lcModel'), function(pred, model, newdata) {
  assert_that(is.newdata(newdata),
              is.null(newdata) || length(pred) == nrow(newdata))
  transformPredict(pred = data.frame(Fit = pred),
                   model = model,
                   newdata = newdata)
})

#' @rdname transformPredict
#' @aliases transformPredict,matrix,lcModel-method
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

#' @rdname transformPredict
#' @aliases transformPredict,data.frame,lcModel-method
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

  if (hasName(newpred, 'Cluster')) {
    newpredClusters = newpred$Cluster
    # drop Cluster column
    newpred = subset(newpred, select = setdiff(names(newpred), 'Cluster')) %>%
      as.data.frame()
    if (hasName(newdata, 'Cluster')) {
      newpred
    } else {
      split(newpred, newpredClusters)
    }
  } else {
    as.data.frame(newpred)
  }
})
