#' @noRd
#' @title Select the specified column per row
#' @keywords internal
.rowColumns = function(x, i) {
  assert_that(
    is.matrix(x),
    all(is.finite(i)),
    length(i) == nrow(x),
    max(i) <= nrow(x) && min(i) >= 1
  )
  x[cbind(1:nrow(x), i)]
}


#' @export
#' @name trajectoryAssignments
#' @aliases trajectoryAssignments,matrix-method
#' @details In case `object` is a `matrix`: the posterior probability `matrix`,
#' with the \eqn{k}th column containing the observation- or trajectory-specific probability for cluster \eqn{k}.
#' @param clusterNames Optional `character vector` with the cluster names.
#' If `clusterNames = NULL`, [make.clusterNames()] is used.
setMethod('trajectoryAssignments', 'matrix', function(
  object,
  strategy = which.max,
  clusterNames = colnames(object),
  ...
) {
  assert_that(is_valid_postprob(object))

  nTraj = nrow(object)
  nClus = ncol(object)

  if (is.null(clusterNames)) {
    clusterNames = make.clusterNames(nClus)
  }

  result = apply(object, 1, strategy, ...)

  assert_that(
    is.numeric(result),
    length(result) == nTraj,
    all(vapply(result, is.count, FUN.VALUE = TRUE) | vapply(result, is.na, FUN.VALUE = TRUE)),
    min(result, na.rm = TRUE) >= 1,
    max(result, na.rm = TRUE) <= nClus
  )

  factor(result, levels = 1:nClus, labels = clusterNames)
})


#' @export
#' @title Convert a multiple time series matrix to a data.frame
#' @param data The `matrix` containing a trajectory on each row.
#' @param id The id column name.
#' @param time The time column name.
#' @param response The response column name.
#' @param ids A `vector` specifying the id names. Should match the number of rows of `data`.
#' @param times A `numeric` `vector` specifying the times of the measurements.
#' Should match the number of columns of `data`.
#' @param as.data.table Whether to return the result as a `data.table`, or a `data.frame` otherwise.
#' @return A `data.table` or `data.frame` containing the repeated measures.
#' @seealso [tsmatrix]
tsframe = function(
    data,
    response,
    id = getOption('latrend.id'),
    time = getOption('latrend.time'),
    ids = rownames(data),
    times = colnames(data),
    as.data.table = FALSE
) {
  assert_that(
    is.matrix(data),
    is.character(id),
    is.character(time),
    is.character(response),
    length(times) == 0 || length(times) == ncol(data),
    length(ids) == 0 || length(ids) == nrow(data),
    noNA(ids),
    noNA(times)
  )

  if (is.character(ids)) {
    ids = factor(ids, levels = ids)
  }
  if (length(ids) == 0) {
    ids = seq_len(nrow(data))
  }

  if (length(times) == 0) {
    times = seq(0, 1, length.out = ncol(data))
  }
  else if (is.factor(times)) {
    suppressWarnings({
      times = as.numeric(as.character(times))
    })
  }
  else if (is.character(times)) {
    suppressWarnings({
      times = as.numeric(times)
    })
  }

  if (anyNA(times)) {
    if (all(!is.finite(times))) {
      times = seq_along(times)
      warning(
        'supplied "times" argument failed to parse to numeric values;
        falling back to times = seq_along(times) instead.'
      )
    } else {
      stop('some time values are non-finite (possible failed to parse to numeric?)')
    }
  }

  dt = data.table(
    Id = rep(ids, each = ncol(data)),
    Time = rep(times, nrow(data)),
    Response = as.numeric(t(data))
  )
  setnames(dt, c('Id', 'Time', 'Response'), c(id, time, response))

  if (as.data.table) {
    dt
  } else {
    as.data.frame(dt)
  }
}

#' @rdname tsframe
#' @note The `meltRepeatedMeasures()` function is deprecated and will be removed in a future version,
#' please use `tsframe()` instead.
meltRepeatedMeasures = tsframe

#' @export
#' @title Convert a longitudinal data.frame to a matrix
#' @description Converts a longitudinal `data.frame` comprising trajectories with an equal number of observations,
#' measured at identical moments in time, to a `matrix`. Each row of the matrix represents a trajectory.
#' @inheritParams tsframe
#' @param fill A `scalar` value.
#' If `FALSE`, an error is thrown when time series observations are missing in the data frame.
#' Otherwise, the value used for representing missing observations.
#' @return A `matrix` with a trajectory per row.
#' @seealso [tsframe]
tsmatrix = function(
    data,
    response,
    id = getOption('latrend.id'),
    time = getOption('latrend.time'),
    fill = NA
) {
  assert_that(
    is.data.frame(data),
    has_name(data, c(id, time, response)),
    nrow(data) > 0,
    is.scalar(fill),
    noNA(data[[id]]),
    noNA(data[[time]])
  )

  dt = as.data.table(data)
  setkeyv(dt, c(id, time))

  ids = unique(dt[[id]])
  times = unique(dt[[time]])

  if (isFALSE(fill)) {
    assert_that(
      are_trajectories_equal_length(dt, id = id, time = time)
    )
  } else if (!are_trajectories_equal_length(dt, id = id, time = time)) {
    # insert missing observations
    dt[, .Fill := FALSE]
    dtIndex = CJ(ids, times)
    setnames(dtIndex, c(id, time))
    dt = dt[dtIndex]
    dt[is.na(.Fill), c(response) := fill]
    dt[, .Fill := NULL]
  }

  dataMat = matrix(
    dt[[response]],
    byrow = TRUE,
    nrow = length(ids),
    ncol = length(times)
  )

  rownames(dataMat) = ids
  colnames(dataMat) = times

  dataMat
}

#' @rdname tsmatrix
#' @note The `dcastRepeatedMeasures()` function is deprecated and will be removed in a future version.
#' Please use `tsmatrix()` instead.
dcastRepeatedMeasures = tsmatrix
