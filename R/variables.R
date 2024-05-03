#. idVariable ####
#' @export
#' @rdname idVariable
#' @aliases idVariable,ANY-method
setMethod('idVariable', 'ANY', function(object) getOption('latrend.id'))


#. timeVariable ####
#' @export
#' @rdname timeVariable
#' @aliases timeariable,ANY-method
setMethod('timeVariable', 'ANY', function(object) getOption('latrend.time'))


#' @title Guess the response variable
#' @description
#' Attempts to identify the response variable for the given trajectory data
#' @param data A trajectories `data.frame`.
#' @param id Id variable(s) to exclude.
#' @param time Time variable(s) to exclude.
#' @param cluster Cluster variable(s) to exclude.
#' @keywords internal
.guessResponseVariable = function(
    data,
  id = getOption('latrend.id'),
  time = getOption('latrend.time'),
  cluster = 'Cluster'
) {
  assert_that(
    is.data.frame(data)
  )

  excludeColumns = unique(c(id, time, cluster))

  numMask = vapply(data, is.numeric, FUN.VALUE = TRUE)
  numericColumns = names(data)[numMask]

  candidates = setdiff(numericColumns, excludeColumns)
  if (length(candidates) == 0L) {
    stop('unable to automatically determine the response variable. Specify the "response" argument.')
  }

  dfNum = subset(data, select = candidates)

  counts = vapply(dfNum, uniqueN, FUN.VALUE = 0L)
  response = names(dfNum)[which.max(counts)]

  message(
    sprintf(
      'Automatically selected "%s" as the response variable.
        To override this, specify the "response" argument.',
      response
    )
  )

  response
}
