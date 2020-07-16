#' @include method.R
#' @title lcMatrixMethod
#' @name lcMatrixMethod
#' @param id Id to use for the rownames of the matrix.
#' @keywords internal
setClass('lcMatrixMethod', contains = 'lcMethod')

setMethod('getName', signature('lcMatrixMethod'), function(object) 'repeated measures model')

setMethod('getShortName', signature('lcMatrixMethod'), function(object) 'rm')

setMethod('prepareData', signature('lcMatrixMethod'), function(method, data, verbose, ...) {
  e = new.env()

  data = as.data.table(data)
  idColumn = idVariable(method)
  timeColumn = timeVariable(method)
  valueColumn = responseVariable(method)

  e$times = sort(unique(data[[timeColumn]]))

  # Check data
  assert_that(uniqueN(data[, .N, by = c(idColumn)]$N) == 1, msg = 'not all time series are of equal length')

  # Data
  cat(
    verbose,
    'Transforming data to aligned repeated measures matrix format...',
    level = verboseLevels$fine
  )
  e$dataMat = dcastRepeatedMeasures(data,
                                    id = idColumn,
                                    time = timeColumn,
                                    response = valueColumn)
  return(e)
})
