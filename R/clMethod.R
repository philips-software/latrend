# Method ####
setClass('clMethod', slots=c(call='call'))

setMethod('initialize', 'clMethod', function(.Object, ...) {
  .Object = callNextMethod()
  validObject(.Object)
  .Object
})

setValidity('clMethod', function(object) {
  call = getCall(object)
  assert_that(is.call(call))
})

setMethod('show', 'clMethod',
          function(object) {
            cat('Cluslong method "', getName(object), '"\n', sep='')
            clMethodPrintArgs(object)
          }
)

#' @export
#' @title Check validity of the arguments in the respective environment.
#' @description Arguments missing from the environment are skipped.
#' @param envir The environment in which to evaluate the arguments.
setGeneric('checkArgs', function(object, envir=parent.frame(), ...) standardGeneric('checkArgs'))
setMethod('checkArgs', signature('clMethod'), function(object, envir) {})

#' @export
#' @title Check whether the argument of a clMethod has a defined value.
#' @keywords internal
isArgDefined = function(object, name, envir=NULL) {
  envir = clMethod.env(object, parent.frame(), envir)
  assert_that(is(object, 'clMethod'))
  assert_that(is.character(name))
  arg = object[[name[1], eval=FALSE]]


  if(is.name(arg) || is.call(arg)) {
    arg = try(object[[name, envir=envir]], silent=TRUE)
    if(is(arg, 'try-error')) {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' @export
setGeneric('getName', function(object) standardGeneric('getName'))
setMethod('getName', signature('clMethod'), function(object) 'custom')

#' @export
setGeneric('getName0', function(object) standardGeneric('getName0'))
setMethod('getName0', signature('clMethod'), getName)


#' @export
#' @title Extract formula
#' @examples
#' m = clMethodKML(Value ~ Time)
#' formula(m) # Value ~ Time
formula.clMethod = function(object, what='mu', envir=NULL) {
  envir = clMethod.env(object, parent.frame(), envir)
  assert_that(is.scalar(what), is.character(what))
  if (what == 'mu') {
    object$formula
  } else {
    object[[paste0('formula.', what)]]
  }
}


#' @title clMethod argument names
#' @examples
#' m = clMethodKML()
#' names(m)
setMethod('names', signature('clMethod'), function(x) {
  names(getCall(x))[-1]
})


#' @title Retrieve and evaluate a clMethod argument by name
#' @param name Name of the argument to retrieve.
#' @examples
#' m = clMethodKML()
#' m$nClusters
setMethod('$', signature('clMethod'), function(x, name) {
  x[[name]]
})


#' @title Retrieve and evaluate a clMethod argument by name
#' @param i Name or index of the argument to retrieve.
#' @param eval Whether to evaluate the call argument (enabled by default).
#' @param envir Environment in which to evaluate the argument. Only applies when eval = TRUE.
#' @examples
#' m = clMethodKML(nClusters=5)
#' m[['nClusters']] # 5
#'
#' K = 2
#' m = clMethodKML(nClusters=K)
#' m[['nClusters', eval=FALSE]] # K
setMethod('[[', signature('clMethod'), function(x, i, eval=TRUE, envir=NULL) {
  envir = clMethod.env(x, parent.frame(3), envir)
  if (is.character(i)) {
    assert_that(has_name(x, i), msg=sprintf('method does not have an argument named "%s"', i))
    arg = getCall(x)[[i]]
  } else {
    argName = names(x)[i]
    assert_that(!is.na(argName), msg=sprintf('index "%s" exceeded argument name options', i))
    arg = getCall(x)[[names(x)[i]]]
  }

  if(eval) {
    value = eval(arg, envir=as.list(envir), enclos=parent.env(getNamespace(.packageName)))
  } else {
    value = arg
  }

  if(is.formula(value)) {
    environment(value) = new.env()
  }

  return(value)
})


#' @export
getCall.clMethod = function(object) {
  object@call
}


#' @export
#' @title Update a method specification
#' @examples
#' m = clMethodKML(Value ~ 1)
#' m2 = update(m, formula=~ . + Time)
#'
#' m3 = update(m2, start='randomAll')
update.clMethod = function(object, ..., envir=NULL) {
  envir = clMethod.env(object, parent.frame(), envir)
  ucall = match.call() %>% tail(-2)
  ucall$envir = NULL
  argNames = names(object)
  uargNames = names(ucall)
  assert_that(all(uargNames %in% argNames),
              msg=sprintf('attempted to update unsupported arguments %s', paste0('"', setdiff(uargNames, argNames), '"', collapse=', ')))
  uargValues = lapply(ucall, eval, envir=envir)
  formulaMask = sapply(uargValues, is, 'formula')

  if(any(formulaMask)) {
    oldFormulaArgs = lapply(uargNames[formulaMask], function(name) object[[name]])
    ucall[formulaMask] = mapply(update.formula, oldFormulaArgs, uargValues[formulaMask], SIMPLIFY=FALSE) %>%
      lapply(match.call, definition=formula)
  }

  object@call = replace(getCall(object), uargNames, ucall[uargNames])
  validObject(object)
  return(object)
}

#' @export
#' @title Extract the method arguments as a list
#' @param eval Whether to evaluate the arguments.
#' @param envir The environment in which to evaluate the arguments.
#' @examples
#' method = clMethodKML()
#' as.list(method)
as.list.clMethod = function(object, eval=TRUE, envir=NULL) {
  envir = clMethod.env(object, parent.frame(), envir)
  if (eval) {
    argNames = names(object)
    argValues = lapply(argNames, function(argName) object[[argName, envir=envir]])
    names(argValues) = argNames
    return(argValues)
  } else {
    as.list(clm@call[-1])
  }
}

#' @export
#' @title Substitute the call arguments
#' @param envir The environment in which to evaluate the arguments.
#' @return A new call with the substituted arguments.
substitute.clMethod = function(object, envir=NULL) {
  envir = clMethod.env(object, parent.frame(), envir)
  assert_that(is(object, 'clMethod'))
  argValues = as.list(object, eval=TRUE, envir=envir)
  object@call = replace(getCall(object), names(argValues), argValues)
  return(object)
}

# Local methods ####
setGeneric('prepare', function(method, ...) standardGeneric('prepare'))
setGeneric('fit', function(method, ...) standardGeneric('fit'))
setGeneric('finalize', function(method, ...) standardGeneric('finalize'))


#' @importFrom R.utils insert
clMethodPrintArgs = function(object) {
  argNames = names(object)
  args = as.list(getCall(object)[-1])[argNames] %>%
    sapply(deparse) %>%
    sapply(strtrim, 40)

  cat(sprintf('  %-16s%s\n', paste0(argNames, ':'), args), sep='')
}

#' @title Select the preferred environment
#' @description Returns envir if specified. Otherwise, returns environment(object) if specified. The defaultEnvir is returned when the former two are NULL.
#' @keywords internal
clMethod.env = function(object, defaultEnvir, envir) {
  assert_that(is(object, 'clMethod'))
  assert_that(is.null(defaultEnvir) || is.environment(defaultEnvir))
  assert_that(is.null(envir) || is.environment(envir))

  if(!is.null(envir)) {
    envir
  } else if(!is.null(environment(object))) {
    environment(object)
  } else {
    defaultEnvir
  }
}
