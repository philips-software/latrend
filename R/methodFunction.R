#' @include method.R

#' @name interface-custom
#' @title function interface
#' @seealso [lcMethodRandom] [lcMethodStratify] [lcModelPartition] [lcModelWeightedPartition]
#' @keywords internal
NULL

setClass('lcMethodFunction', contains = 'lcMethod')


#' @export
#' @title Specify a custom method based on a function
#' @param fun The cluster `function` with signature `(method, data)` that returns a `lcModel` object.
#' @param center Optional `function` for computing the longitudinal cluster centers, with signature `(x)`.
#' @param response The name of the response variable.
#' @param time The name of the time variable.
#' @param id The name of the trajectory identification variable.
#' @param name The name of the method.
#' @examples
#' data(latrendData)
#' # Stratification based on the mean response level
#' clusfun <- function(data, response, id, time, ...) {
#'   clusters <- data.table::as.data.table(data)[, mean(Y) > 0, by = Id]$V1
#'   lcModelPartition(
#'     data = data,
#'     trajectoryAssignments = factor(
#'       clusters,
#'       levels = c(FALSE, TRUE),
#'       labels = c("Low", "High")
#'     ),
#'     response = response,
#'     time = time,
#'     id = id
#'   )
#' }
#' method <- lcMethodFunction(response = "Y", fun = clusfun, id = "Id", time = "Time")
#' model <- latrend(method, data = latrendData)
#' @family lcMethod implementations
lcMethodFunction = function(
  response,
  fun,
  center = meanNA,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  name = 'custom'
) {
  mc = match.call.all()
  mc$Class = 'lcMethodFunction'
  do.call(new, as.list(mc))
}

setValidity('lcMethodFunction', function(object) {
  assert_that(has_lcMethod_args(object, formalArgs(lcMethodFunction)))

  if (isArgDefined(object, 'fun')) {
    assert_that(is.function(object$fun))
  }

  if (isArgDefined(object, 'center')) {
    assert_that(is.function(object$center))
  }
})

#' @rdname interface-custom
setMethod('getArgumentDefaults', 'lcMethodFunction', function(object) {
  c(
    formals(lcMethodFunction),
    callNextMethod()
  )
})

#' @rdname interface-custom
#' @inheritParams getName
setMethod('getName', 'lcMethodFunction', function(object) {
  if (isArgDefined(object, 'name') && !is.null(object$name)) {
    return (object$name)
  }

  if (isArgDefined(object, 'fun')) {
    fun = object[['fun', eval = FALSE]]
    if (is.name(fun)) {
      return (paste('custom function ', fun))
    }
  }

  'custom function'
})

#' @rdname interface-custom
setMethod('getShortName', 'lcMethodFunction', function(object) 'custom')

#' @rdname interface-custom
setMethod('prepareData', 'lcMethodFunction', function(method, data, verbose) {
  assert_that(has_name(data, responseVariable(method)))
  callNextMethod()
})

#' @rdname interface-custom
#' @inheritParams fit
setMethod('fit', 'lcMethodFunction', function(method, data, envir, verbose) {
  args = as.list(method)
  args$data = data

  model = do.call(method$fun, args)
  assert_that(
    is.lcModel(model),
    msg = 'Invalid output from the function defined for this lcMethodFunction object. The function should return an object of class lcModel'
  )

  model@method = method

  model
})
