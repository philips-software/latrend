#' @include clMethod.R
#' @title clMatrixMethod
#' @name clMatrixMethod
#' @param id Id to use for the rownames of the matrix.
#' @keywords internal
setClass('clMatrixMethod', contains='clMethod')

setMethod('getName', signature('clMatrixMethod'), function(object) 'repeated measures model')

setMethod('getName0', signature('clMatrixMethod'), function(object) 'rm')

setMethod('prepare', signature('clMatrixMethod'), function(method, data) {
  e = new.env()
  data = as.data.table(data)
  valueColumn = formula(method) %>% getResponse

  e$times = sort(unique(data[[method$time]]))

  # Check data
  assert_that(uniqueN(data[, .N, by=c(method$id)]$N) == 1, msg='not all time series are of equal length')

  # Data
  logfine('Reshaping data...')
  e$dataMat = dcastRepeatedMeasures(data, id=method$id, time=method$time, response=valueColumn)
  return(e)
})
