#' @include method.R latrend.R
#' @title lcMatrixMethod
#' @name lcMatrixMethod-class
#' @rdname lcMatrixMethod-class
#' @inheritParams trajectories
#' @keywords internal
setClass('lcMatrixMethod', contains = 'lcMethod')

#' @rdname lcMatrixMethod-class
#' @inheritParams getName
setMethod('getName', 'lcMatrixMethod', function(object, ...) 'repeated measures model')

#' @rdname lcMatrixMethod-class
setMethod('getShortName', 'lcMatrixMethod', function(object, ...) 'rm')

#' @rdname lcMatrixMethod-class
#' @inheritParams prepareData
setMethod('prepareData', 'lcMatrixMethod', function(method, data, verbose, ...) {
  e = new.env()

  data = as.data.table(data)
  idColumn = idVariable(method)
  timeColumn = timeVariable(method)
  valueColumn = responseVariable(method)

  e$times = sort(unique(data[[timeColumn]]))

  # Data
  cat(
    verbose,
    'Transforming data to aligned repeated measures matrix format...',
    level = verboseLevels$fine
  )
  e$dataMat = tsmatrix(
    data,
    id = idColumn,
    time = timeColumn,
    response = valueColumn
  )

  e
})
