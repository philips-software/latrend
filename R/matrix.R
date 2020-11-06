#' @noRd
#' @title Select the specified column per row
#' @keywords internal
rowColumns = function(x, i) {
  assert_that(is.matrix(x) && all(is.finite(i)))
  assert_that(length(i) == nrow(x) &&
                max(i) <= nrow(x) && min(i) >= 1)
  x[cbind(1:nrow(x), i)]
}

#' @export
#' @title Convert a repeated measures data matrix to a data.frame
#' @param data The `matrix` containing a trajectory on each row.
#' @param id The id column name.
#' @param time The time column name.
#' @param response The response column name.
#' @param ids A `vector` specifying the id names. Should match the number of rows of `data`.
#' @param times A `numeric` `vector` specifying the times of the measurements. Should match the number of columns of `data`.
#' @param as.data.table Whether to return the result as a `data.table`, or a `data.frame` otherwise.
#' @return A `data.table` or `data.frame` containing the repeated measures.
meltRepeatedMeasures = function(data,
                                response,
                                id = getOption('latrend.id'),
                                time = getOption('latrend.time'),
                                ids = rownames(data),
                                times = colnames(data),
                                as.data.table = FALSE) {
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
        'supplied "times" argument failed to parse to numeric values; falling back to times = seq_along(times) instead.'
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

#' @export
#' @title Cast a longitudinal data.frame to a matrix
#' @description Converts a longitudinal `data.frame` comprising trajectories with an equal number of observations, measured at identical moments in time, to a `matrix`. Each row of the matrix represents a trajectory.
#' @inheritParams meltRepeatedMeasures
#' @return A `matrix` with a trajectory per row.
dcastRepeatedMeasures = function(data,
                                 response,
                                 id = getOption('latrend.id'),
                                 time = getOption('latrend.time')) {
  assert_that(has_name(data, c(id, time, response)))

  dt = as.data.table(data)
  setkeyv(dt, c(id, time))

  numIds = uniqueN(dt[[id]])
  numTime = uniqueN(dt[[time]])
  assert_that(all(dt[, .N, by=c(id)]$N == numTime), msg = 'trajectories do not all have the same number of observations')

  dataMat = matrix(dt[[response]],
                   byrow = TRUE,
                   nrow = uniqueN(dt[[id]]),
                   ncol = uniqueN(dt[[time]]))

  rownames(dataMat) = unique(dt[[id]])
  colnames(dataMat) = unique(dt[[time]])
  return(dataMat)
}
