#' @include method.R

#' @export
#' @name lcMetaMethods
#' @rdname lcMetaMethods
#' @aliases lcMetaMethod-class
#' @title Meta methods
#' @description `lcMetaMethod` classes are used to specify a repeated or adjusted fitting approach for the given longitudinal cluster method.
#' Supported meta methods:
#' * `lcMetaMethodConverged`: Fit a method until a converged result is obtained.
setClass(
  'lcMetaMethod',
  contains = c('lcMethod', 'VIRTUAL')
)

as.character.lcMetaMethod = function(x, ...) {
  c(
    sprintf('%s encapsulating:', class(x)[1]),
    paste0(' ', as.character(getLcMethod(x), ...)),
    ' with meta-method arguments:',
    paste0('  ', tail(as.character.lcMethod(x), -2L))
  )
}

#' @export
#' @name lcMetaMethod-interface
#' @rdname lcMetaMethod-interface
#' @title lcMetaMethod methods
setMethod('compose', 'lcMetaMethod', function(method, envir = NULL) {
  newMethod = method
  newMethod@arguments$method = evaluate.lcMethod(getLcMethod(method), try = FALSE, envir = envir)
  newMethod
})

#' @export
#' @rdname lcMetaMethod-interface
setMethod('getLcMethod', 'lcMetaMethod', function(object, ...) object$method)

#' @export
#' @rdname lcMetaMethod-interface
setMethod('getName', 'lcMetaMethod', function(object, ...) getName(getLcMethod(object), ...))

#' @export
#' @rdname lcMetaMethod-interface
setMethod('getShortName', 'lcMetaMethod', function(object, ...) getShortName(getLcMethod(object), ...))

#' @export
#' @rdname idVariable
setMethod('idVariable', 'lcMetaMethod', function(object, ...) idVariable(getLcMethod(object), ...))

#' @export
#' @rdname lcMetaMethod-interface
setMethod('preFit', 'lcMetaMethod', function(method, data, envir, verbose) {
  preFit(getLcMethod(method), data = data, envir = envir, verbose = verbose)
})

#' @export
#' @rdname lcMetaMethod-interface
setMethod('prepareData', 'lcMetaMethod', function(method, data, verbose) {
  prepareData(getLcMethod(method), data = data, verbose = verbose)
})

#' @export
#' @rdname lcMetaMethod-interface
setMethod('postFit', 'lcMetaMethod', function(method, data, model, envir, verbose) {
  postFit(getLcMethod(method), data = data, model = model, envir = envir, verbose = verbose)
})

#' @export
#' @rdname responseVariable
setMethod('responseVariable', 'lcMetaMethod', function(object, ...) responseVariable(getLcMethod(object), ...))

#' @export
#' @rdname timeVariable
setMethod('timeVariable', 'lcMetaMethod', function(object, ...) timeVariable(getLcMethod(object), ...))

#' @export
#' @rdname lcMetaMethod-interface
setMethod('validate', 'lcMetaMethod', function(method, data, envir = NULL, ...) {
  validate(getLcMethod(method), data = data, envir = envir, ...)
})
