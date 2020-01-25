#' @title Select the specified column per row
#' @keywords internal
rowColumns = function(x, i) {
  assert_that(is.matrix(x) && all(is.finite(i)))
  assert_that(length(i) == nrow(x) && max(i) <= nrow(x) && min(i) >= 1)
  x[cbind(1:nrow(x), i)]
}

#' @export
#' @title Convert a repeated measures data matrix to a data.frame
#' @param data The `matrix` containing a trajectory on each row.
#' @param id The id column name.
#' @param time The time column name.
#' @param response The response column name.
#' @param ids A `vector` specifying the id names. Should match the number of rows of `data`.
#' @param times A `vector` specifying the times of the measurements. Should match the number of columns of `data`.
#' @param as.data.table Whether to return the result as a `data.table`, or a `data.frame` otherwise.
#' @return A `data.table` or `data.frame` containing the repeated measures.
meltRepeatedMeasures = function(data,
                                  id=getOption('cluslong.id'),
                                  time=getOption('cluslong.time'),
                                  response=getOption('cluslong.response'),
                                  ids=rownames(data),
                                  times=as.numeric(colnames(data)),
                                  as.data.table=FALSE) {
  assert_that(is.matrix(data))
  assert_that(is.character(id), is.character(time), is.character(response))

  ids = rownames(data)
  times = colnames(data)

  if(length(ids) == 0) {
    ids = seq_len(nrow(data))
  }
  if(length(times) == 0) {
    times = seq(0, 1, length.out=ncol(data))
  }

  assert_that(length(ids) == nrow(data), length(times) == ncol(data))

  dt = data.table(Id=rep(ids, each=ncol(data)),
             Time=rep(times, nrow(data)),
             Response=as.numeric(t(data)))
  setnames(dt, c('Id', 'Time', 'Response'), c(id, time, response))

  if(as.data.table) {
    dt
  } else {
    as.data.frame(dt)
  }
}

#' @export
#' @title Cast a longitudinal data.frame to a matrix
#' @inheritParams meltRepeatedMeasures
#' @return A `matrix` of the repeated measures.
dcastRepeatedMeasures = function(data,
                                 id=getOption('cluslong.id'),
                                 time=getOption('cluslong.time'),
                                 response=getOption('cluslong.response')) {
  assert_that(has_name(data, c(id, time, response)))
  df = reshape2::dcast(data, formula=as.formula(paste(id, time, sep='~')), value.var=response, fill=NA*0, fun.aggregate=mean)
  dataMat = as.matrix(df[, -1])
  rownames(dataMat) = df[[1]]
  colnames(dataMat) = sort(unique(data[[time]]))
  return(dataMat)
}