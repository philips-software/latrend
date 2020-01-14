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
#' @family clMethod
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
#' @family clMethod
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
#' @family clMethod
setMethod('names', signature('clMethod'), function(x) {
  names(getCall(x))[-1]
})

#' @title Length of a clMethod object
#' @description Extracts the number of specified arguments of a `clMethod` object.
#' @examples
#' m = clMethodKML()
#' length(m)
#' @family clMethod
setMethod('length', signature('clMethod'), function(x) {
  length(getCall(x)) - 1
})


#' @title Retrieve and evaluate a clMethod argument by name
#' @param name Name of the argument to retrieve.
#' @examples
#' m = clMethodKML()
#' m$nClusters
#' @family clMethod
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
#' @family clMethod
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
#' @family clMethod
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
#' @family clMethod
update.clMethod = function(object, ..., envir=NULL) {
  envir = clMethod.env(object, parent.frame(), envir)
  ucall = match.call() %>% tail(-2)
  ucall$envir = NULL
  argNames = names(object)
  uargNames = names(ucall)
  uargValues = lapply(ucall, eval, envir=envir)
  defMask = uargNames %in% argNames
  formulaMask = vapply(uargValues, is, 'formula', FUN.VALUE=FALSE)
  updateFormulaMask = formulaMask & defMask

  if(any(updateFormulaMask)) {
    oldFormulaArgs = lapply(uargNames[updateFormulaMask], function(name) object[[name]])
    ucall[updateFormulaMask] = mapply(update.formula, oldFormulaArgs, uargValues[updateFormulaMask], SIMPLIFY=FALSE) %>%
      lapply(match.call, definition=formula)
  }

  object@call = replace(getCall(object), uargNames, ucall[uargNames])
  validObject(object)
  return(object)
}

#' @export
#' @title Extract the method arguments as a list
#' @param eval Whether to evaluate the arguments. Alternatively, a character vector of class names of the values that are accepted for replacement, in which case evaluation does not throw an error if an argument is not defined.
#' @param envir The environment in which to evaluate the arguments.
#' @examples
#' method = clMethodKML()
#' as.list(method)
#' @family clMethod
as.list.clMethod = function(object, eval=TRUE, envir=NULL) {
  envir = clMethod.env(object, parent.frame(), envir)
  if(isTRUE(eval) || is.scalar(eval) && eval == 'ANY') {
    # full evaluation
    argNames = names(object)
    argValues = lapply(argNames, function(argName) object[[argName, envir=envir]])
    names(argValues) = argNames
    return(argValues)
  } else if(is.character(eval)) {
    # partial update
    argNames = names(object)
    argValues = as.list(object@call[-1])
    evalValues = vector(mode='list', length=length(object))
    evalMask = sapply(argNames, isArgDefined, object=object, envir=envir)
    evalValues[evalMask] = lapply(argNames[evalMask], function(name) object[[name, eval=TRUE, envir=envir]])
    updateMask = evalMask & sapply(evalValues, class) %in% eval
    argValues[updateMask] = evalValues[updateMask]
    return(argValues)
  } else {
    as.list(object@call[-1])
  }
}

#' @export
#' @title Convert clMethod arguments to a list of atomic types
#' @description Converts the arguments of a `clMethod` to a named `list` of [atomic] types.
#' @inheritParams as.list.clMethod
#' @param x `clMethod` to be coerced to a `character` `vector`.
#' @param eval Whether to evaluate the arguments in order to replace expression if the resulting value is of a class specified in `evalClasses`.
#' @param nullValue Value to use to represent the `NULL` type. Must be of length 1.
#' @family clMethod
as.data.frame.clMethod = function(x,
                                eval=FALSE,
                                envir=NULL,
                                nullValue=NA) {
  assert_that(is.logical(eval))
  assert_that(length(nullValue) == 1)
  envir = clMethod.env(x, parent.frame(), envir)

  evalClasses = c('NULL', 'logical', 'numeric', 'complex', 'integer', 'character', 'factor')
  argValues = as.list(x, eval=if(eval) evalClasses else FALSE, envir=envir)

  dfList = lapply(argValues, function(x) {
    if(is.null(x)) {
      nullValue
    } else if(is.atomic(x)) {
      if(length(x) > 1) {
        dput(x)
      } else {
        x
      }
    } else {
      chr = as.character(x)
      if(length(chr) > 1) {
        deparse(x)
      } else {
        chr
      }
    }
  })

  if(any(sapply(dfList, length) != 1)) {
    browser()
  }

  assert_that(all(sapply(dfList, length) == 1))
  as.data.frame(dfList, stringsAsFactors=FALSE)
}

#' @export
#' @title Convert clMethod arguments to character vector
#' @description Converts the arguments of a `clMethod` to a named `character` vector.
#' @inheritParams as.data.frame.clMethod
#' @param nullString Character to use to represent NULL values.
#' @seealso as.data.frame.clMethod
#' @family clMethod
as.character.clMethod = function(x,
                                 eval=FALSE,
                                 envir=NULL,
                                 nullString='NULL') {
  envir = clMethod.env(x, parent.frame(), envir)
  evalClasses = c('NULL', 'logical', 'numeric', 'complex', 'integer', 'character', 'factor')
  argValues = as.list(x, eval=if(eval) evalClasses else FALSE, envir=envir)

  chrValues = lapply(argValues, function(x) {
    if(is.null(x)) {
      nullString
    } else if(is.atomic(x)) {
      if(length(x) > 1) {
        as.character(x) %>% paste0(collapse=', ')
      } else {
        as.character(x)
      }
    } else {
      chr = as.character(x)
      if(length(chr) > 1) {
        deparse(x)
      } else {
        chr
      }
    }
  })

  assert_that(all(sapply(chrValues, length) == 1))
  unlist(chrValues)
}

#' @title Substitute the call arguments
#' @param envir The environment in which to evaluate the arguments.
#' @return A new call with the substituted arguments.
#' @family clMethod
#' @keywords internal
substitute.clMethod = function(object, envir=NULL) {
  envir = clMethod.env(object, parent.frame(), envir)
  assert_that(is(object, 'clMethod'))
  argValues = as.list(object, eval=TRUE, envir=envir)
  object@call = replace(getCall(object), names(argValues), argValues)
  return(object)
}

# Local methods ####
#' @title clMethod interface function
#' @description Called by [cluslong].
setGeneric('prepare', function(method, ...) standardGeneric('prepare'))
#' @title clMethod interface function
#' @description Called by [cluslong].
setGeneric('fit', function(method, ...) standardGeneric('fit'))
#' @title clMethod interface function
#' @description Called by [cluslong].
setGeneric('finalize', function(method, ...) standardGeneric('finalize'))


clMethodPrintArgs = function(object, ...) {
  argNames = names(object)
  args = as.character(object, ...) %>%
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
