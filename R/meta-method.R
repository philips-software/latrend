#' @include method.R

#' @export
#' @name interface-metaMethods
#' @rdname interface-metaMethods
#' @inheritParams lcMethod-class
#' @inheritParams getLcMethod
#' @inheritParams compose
#' @inheritParams preFit
#' @inheritParams postFit
#' @inheritParams prepareData
#' @inheritParams validate
#' @aliases lcMetaMethod-class
#' @title lcMetaMethod abstract class
#' @description Virtual class for internal use. Do not use.
setClass(
  'lcMetaMethod',
  contains = c('lcMethod', 'VIRTUAL'),
  slots = c(method = 'lcMethod')
)


#' @exportS3Method base::as.character
as.character.lcMetaMethod = function(x, ...) {
  c(
    sprintf('%s encapsulating:', class(x)[1]),
    paste0(' ', as.character(getLcMethod(x), ...)),
    ' with meta-method arguments:',
    paste0('  ', as.character.lcMethod(x))
  )
}


#' @exportS3Method stats::getCall
getCall.lcMetaMethod = function(x, ...) {
  do.call(
    call,
    c(
      name = class(x)[1],
      c(
        method = quote(getCall(getLcMethod(x))),
        eapply(x@arguments, enquote)
      )
    )
  )
}


#' @export
#' @rdname interface-metaMethods
setMethod('compose', 'lcMetaMethod', function(method, envir = NULL) {
  newMethod = method
  newMethod@method = evaluate.lcMethod(getLcMethod(method), try = FALSE, envir = envir)
  newMethod
})


#' @export
#' @rdname interface-metaMethods
setMethod('getLcMethod', 'lcMetaMethod', function(object, ...) {
  assert_that(
    is.lcMethod(object@method),
    msg = 'meta-method implementation error: underlying method argument is not a lcMethod object'
  )
  object@method
})


#' @export
#' @rdname interface-metaMethods
setMethod('getName', 'lcMetaMethod',
  function(object, ...) getName(getLcMethod(object), ...)
)


#' @export
#' @rdname interface-metaMethods
setMethod('getShortName', 'lcMetaMethod',
  function(object, ...) getShortName(getLcMethod(object), ...)
)


#' @export
#' @rdname interface-metaMethods
setMethod('idVariable', 'lcMetaMethod', function(object, ...) idVariable(getLcMethod(object), ...))


#' @export
#' @rdname interface-metaMethods
setMethod('preFit', 'lcMetaMethod', function(method, data, envir, verbose) {
  preFit(getLcMethod(method), data = data, envir = envir, verbose = verbose)
})


#' @export
#' @rdname interface-metaMethods
setMethod('prepareData', 'lcMetaMethod', function(method, data, verbose) {
  prepareData(getLcMethod(method), data = data, verbose = verbose)
})


#' @export
#' @rdname interface-metaMethods
setMethod('fit', 'lcMetaMethod', function(method, data, envir, verbose) {
  fit(getLcMethod(method), data = data, envir = envir, verbose = verbose)
})


#' @export
#' @rdname interface-metaMethods
setMethod('postFit', 'lcMetaMethod', function(method, data, model, envir, verbose) {
  postFit(getLcMethod(method), data = data, model = model, envir = envir, verbose = verbose)
})


#' @export
#' @rdname interface-metaMethods
setMethod('responseVariable', 'lcMetaMethod',
  function(object, ...) responseVariable(getLcMethod(object), ...)
)


#' @export
#' @rdname interface-metaMethods
setMethod('timeVariable', 'lcMetaMethod',
  function(object, ...) timeVariable(getLcMethod(object), ...)
)


#' @export
#' @rdname interface-metaMethods
setMethod('validate', 'lcMetaMethod', function(method, data, envir = NULL, ...) {
  validate(getLcMethod(method), data = data, envir = envir, ...)
})


#' @exportS3Method stats::update
#' @rdname interface-metaMethods
#' @inheritParams update.lcMethod
update.lcMetaMethod = function(object, ...) {
  assert_that(inherits(object, 'lcMetaMethod'))

  object@method = update(getLcMethod(object), ...)

  object
}
