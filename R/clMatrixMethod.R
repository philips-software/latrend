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

  # Check data
  if(!hasName(method, 'approx') || is.null(method$approx)) {
    assert_that(uniqueN(data[, .N, by=c(method$id)]$N) == 1, msg='not all time series are of equal length')
  }


  # Data
  logfine('Reshaping data...')
  wideFrame = dcast(data, as.formula(paste(method$id, method$time, sep='~')), value.var=valueColumn)
  e$dataMat = as.matrix(wideFrame[, -1]) %>%
    set_rownames(wideFrame[[1]])
  return(e)
})
