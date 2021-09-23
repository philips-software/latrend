#' @include model.R

# transformFitted ####
#' @export
#' @rdname transformFitted
#' @title Helper function for custom lcModel classes implementing fitted.lcModel()
#' @description A helper function for implementing the [fitted.lcModel()] method as part of your own `lcModel` class, ensuring the correct output type and format (see the Value section).
#' Note that this function has no use outside of implementing `fitted.lcModel`.
#'
#' The function makes it easier to implement `fitted.lcModel` based on existing implementations that may output their results in different data formats. Furthermore, the function checks whether the input data is valid.
#'
#' The prediction ordering depends on the ordering of the data observations that was used for fitting the `lcModel`.
#'
#' By default, `transformFitted()` accepts one of the following inputs:
#' \describe{
#'  \item{`data.frame`}{A `data.frame` in long format providing a cluster-specific prediction for each observation per row, with column names `"Fit"` and `"Cluster"`. This `data.frame` therefore has `nobs(object) * nClusters(object)` rows.}
#'  \item{`matrix`}{An N-by-K `matrix` where each row provides the cluster-specific predictions for the respective observation. Here, `N = nobs(object)` and `K = nClusters(object)`.}
#'  \item{`list`}{A `list` of cluster-specific prediction `vector`s. Each prediction vector should be of length `nobs(object)`. The overall (named) list of cluster-specific prediction vectors is of length `nClusters(object)`.}
#' }
#'
#' Users can implement support for other prediction formats by defining the `transformFitted` method with other signatures.
#'
#' @section Example implementation:
#' A typical implementation of [fitted.lcModel()] for your own `lcModel` class would have the following format:
#' \preformatted{
#' fitted.lcModelExample <- function(object,
#'  clusters = trajectoryAssignments(object)) {
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
setGeneric('transformFitted', function(pred, model, clusters = NULL) {
  if (!is.null(clusters)) {
    assert_that(
      is.atomic(clusters),
      length(clusters) == nIds(model),
      noNA(clusters),
      all(clusters %in% clusterNames(model))
    )
  }

  out <- standardGeneric('transformFitted')

  if (is.null(clusters)) {
    assert_that(
      is.matrix(out),
      is.numeric(out),
      ncol(out) == nClusters(model)
    )
    colnames(out) = clusterNames(model)
  } else {
    assert_that(
      is.numeric(out)
    )
  }

  return (out)
})

#' @rdname transformFitted
#' @aliases transformFitted,NULL,lcModel-method
setMethod('transformFitted', signature('NULL', 'lcModel'), function(pred, model, clusters) {
  suppressWarnings({
    newpred = matrix(NA_real_, nrow = nobs(model), ncol = nClusters(model))
  })
  colnames(newpred) = clusterNames(model)

  transformFitted(newpred, model, clusters)
})

#' @rdname transformFitted
#' @aliases transformFitted,matrix,lcModel-method
setMethod('transformFitted', signature('matrix', 'lcModel'), function(pred, model, clusters) {
  suppressWarnings(
    assert_that(
      is.numeric(pred),
      ncol(pred) == nClusters(model),
      nrow(pred) == nobs(model),
      !is.null(colnames(pred)),
      noNA(colnames(pred)),
      all(clusterNames(model) %in% colnames(pred))
    )
  )

  newOrder = match(colnames(pred), clusterNames(model))
  pred = pred[, newOrder, drop = FALSE]

  if (is.null(clusters)) {
    pred
  } else if (nrow(pred) == 0) {
    numeric()
  } else {
    clusters = make.clusterIndices(model, clusters)
    rowClusters = clusters[make.idRowIndices(model)]
    rowColumns(pred, rowClusters)
  }
})

#' @rdname transformFitted
#' @aliases transformFitted,list,lcModel-method
setMethod('transformFitted', signature('list', 'lcModel'), function(pred, model, clusters) {
  assert_that(
    length(pred) == nClusters(model),
    is_named(pred),
    has_name(pred, clusterNames(model))
  )

  newpred = lapply(pred, '[[', 'Fit') %>%
    do.call(cbind, .)

  transformFitted(newpred, model, clusters)
})

#' @rdname transformFitted
#' @aliases transformFitted,data.frame,lcModel-method
setMethod('transformFitted', signature('data.frame', 'lcModel'), function(pred, model, clusters) {
  assert_that(
    has_name(pred, c('Fit', 'Cluster')),
    noNA(pred$Cluster),
    all(clusterNames(model) %in% pred$Cluster)
  )

  assert_that(
    all(aggregate(. ~ Cluster, pred, length)$Fit == nobs(model)),
    msg = sprintf('The number of rows per cluster does not match the number of models observations: Expecting %d rows per cluster', nobs(model))
  )

  predmat = unstack(pred, Fit ~ Cluster) %>% as.matrix()
  assert_that(all(colnames(predmat) == clusterNames(model)))

  transformFitted(predmat, model, clusters)
})


# transformPredict ####
#' @export
#' @rdname transformPredict
#' @title Helper function for custom lcModel classes implementing predict.lcModel()
#' @description A helper function for implementing the [predict.lcModel()][predict.lcModel] method as part of your own `lcModel` class, ensuring the correct output type and format (see the Value section).
#' Note that this function has no use outside of ensuring output for `predict.lcModel`. For implementing `lcModel` predictions from scratch, it is advisable to implement [predictForCluster] instead of [predict.lcModel].
#'
#' The prediction ordering corresponds to the observation ordering of the `newdata` argument.
#'
#' By default, `transformPredict()` accepts one of the following inputs:
#' \describe{
#'  \item{`data.frame`}{A `data.frame` in long format providing a cluster-specific prediction for each observation per row, with column names `"Fit"` and `"Cluster"`. This `data.frame` therefore has `nobs(object) * nClusters(object)` rows.}
#'  \item{`matrix`}{An N-by-K `matrix` where each row provides the cluster-specific predictions for the respective observations in `newdata`. Here, `N = nrow(newdata)` and `K = nClusters(object)`.}
#'  \item{`vector`}{A `vector` of length `nrow(newdata)` with predictions corresponding to the rows of `newdata`.}
#' }
#'
#' Users can implement support for other prediction formats by defining the `transformPredict()` method with other signatures.
#'
#' @section Example implementation:
#' In case we have a custom `lcModel` class based on an existing internal model representation with a `predict()` function,
#' we can use `transformPredict()` to easily transform the internal model predictions to the right format.
#' A common output is a `matrix` with the cluster-specific predictions.
#' \preformatted{
#' predict.lcModelExample <- function(object, newdata) {
#'   predictionMatrix <- predict(object@model, newdata)
#'   transformPredict(
#'     pred = predictionMatrix,
#'     model = object,
#'     newdata = newdata)
#' }
#' }
#'
#' However, for ease of implementation it is generally advisable to implement [predictForCluster] instead of [predict.lcModel].
#'
#' For a complete and runnable example, see the custom models vignette accessible via \code{vignette("custom", package = "latrend")}.
#'
#' @usage transformPredict(pred, model, newdata)
#' @param pred The (per-cluster) predictions for `newdata`.
#' @param model The `lcModel` for which the prediction was made.
#' @param newdata A `data.frame` containing the input data to predict for.
#' @return A `data.frame` with the predictions, or a list of cluster-specific prediction `data.frame`s.
#' @seealso predictForCluster, predict.lcModel
setGeneric('transformPredict', function(pred, model, newdata) {
  assert_that(
    is.data.frame(newdata)
  )

  out <- standardGeneric('transformPredict')

  df_validate = function(df) {
    validate_that(
      has_name(df, 'Fit'),
      is.numeric(df$Fit),
      nrow(df) == nrow(newdata)
    )
  }

  if (is.data.frame(out)) {
    valid = df_validate(out)

    if (!isTRUE(valid)) {
      stop(sprintf(
        'implementation error in transformPredict(): %s',
        valid
      ))
    }
  } else {
    assert_that(
      is.list(out),
      all(vapply(out, is.data.frame, FUN.VALUE = TRUE)),
      msg = 'implementation error in transformPredict(): expecting data.frame or list of data.frames output'
    )
    assert_that(all(names(out) == clusterNames(model)), msg = 'expecting list of data.frames named after the clusters')

    valids = sapply(out, df_validate)
    validMask = vapply(valids, isTRUE, FUN.VALUE = TRUE)

    if (!all(validMask)) {
      stop(
        'wrong format for one or more data.frames in the data.frame cluster list output: ',
        paste0(valids[!validMask], collapse = '+ \n')
      )
    }
  }

  return (out)
})

#' @rdname transformPredict
#' @aliases transformPredict,NULL,lcModel-method
setMethod('transformPredict', signature('NULL', 'lcModel'), function(pred, model, newdata) {
  assert_that(
    nrow(newdata) == 0,
    msg = sprintf(
      '%s implementation error: transformPredict(NULL, ...) can only be used when newdata is empty',
      class(model)[1]
    )
  )

  if (hasName(newdata, 'Cluster')) {
    data.frame(
      Cluster = factor(levels = seq_len(nClusters(model)), labels = clusterNames(model)),
      Fit = numeric(0)
    )
  } else {
    data.frame(Fit = numeric(0))
  }
})

#' @rdname transformPredict
#' @aliases transformPredict,vector,lcModel-method
setMethod('transformPredict', signature('vector', 'lcModel'), function(pred, model, newdata) {
  assert_that(is.null(newdata) || length(pred) == nrow(newdata))

  transformPredict(
    pred = data.frame(Fit = pred),
    model = model,
    newdata = newdata
  )
})

#' @rdname transformPredict
#' @aliases transformPredict,matrix,lcModel-method
setMethod('transformPredict', signature('matrix', 'lcModel'), function(pred, model, newdata) {
  # format where multiple cluster-specific predictions are given per newdata entry (per row)
  assert_that(
    is.matrix(pred),
    ncol(pred) == nClusters(model),
    is.null(newdata) || nrow(pred) == nrow(newdata)
  )

  if (hasName(newdata, 'Cluster')) {
    rowClusters = make.clusterIndices(model, newdata$Cluster)
    data.frame(Fit = rowColumns(pred, rowClusters))
  } else {
    data.frame(Fit = as.vector(pred)) %>%
      split(clusterNames(model, factor = TRUE) %>% rep(each = nrow(pred)))
  }
})

#' @rdname transformPredict
#' @aliases transformPredict,data.frame,lcModel-method
setMethod('transformPredict', signature('data.frame', 'lcModel'), function(pred, model, newdata) {
  assert_that(
    !is.null(newdata),
    msg = sprintf('%s implementation error: newdata cannot be NULL for data.frame input', class(model)[1])
  )

  # generic form, possibly containing more predictions than newdata. These are filtered
  # if the pred object contains the newdata variables. Else, newdata is replicated.
  pred = as.data.table(pred)
  newdata = as.data.table(newdata)

  if (nrow(newdata) == 0) {
    return (as.data.table(pred)[0, ])
  }

  assert_that(
    has_name(pred, 'Fit'),
    nrow(pred) > 0
  )

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

    assert_that(
      all(obsCounts[is.finite(N) & is.finite(i.N), N == i.N]),
      msg = 'number of observations per cluster must match the number of predictions per cluster'
    )

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
