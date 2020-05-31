# Method ####
setClass('clMethod', slots=c(call='call'))

setMethod('initialize', 'clMethod', function(.Object, ...) {
  .Object = callNextMethod()
  validObject(.Object)
  .Object
})

setValidity('clMethod', function(object) {
  call = getCall(object)
  assert_that(!any(vapply(names(object), startsWith, '.', FUN.VALUE=TRUE)), msg='clMethod argument names cannot start with "."')

  if(isArgDefined(object, 'formula')) {
    assert_that(is.formula(object$formula))
  }

  if(isArgDefined(object, 'nClusters')) {
    assert_that(is.na(object$nClusters) || is.count(object$nClusters))
  }
})

setMethod('show', 'clMethod',
          function(object) {
            cat('Cluslong method "', getName(object), '"\n', sep='')
            print(object)
          }
)

#' @export
#' @title Construct a clMethod object for a given implementation
#' @description Creates a clMethod class of the specified type `Class` for the given arguments given in a call, along with any default arguments from reference functions.
#' @param Class The type of `clMethod` class
#' @param call The arguments to create the `clMethod` from.
#' @param defaults List of `function` to obtain defaults from for arguments unspecified by `call`.
#' @param excludeArgs The names of the arguments to exclude from the defaults, provided as a `character vector`.
#' @examples
#' clMethodKML2 = function(formula=Value ~ 0, time='Id', id='Id', nClusters=2, ...) {
#'   clMethod('clMethodKML', call=stackoverflow::match.call.defaults(),
#'     defaults=c(kml::kml, kml::parALGO),
#'     excludeArgs=c('object', 'nbClusters', 'parAlgo', 'toPlot', 'saveFreq'))
#' }
#' clMethodKML2(nClusters=3)
#' @family clMethod functions
clMethod = function(Class, call, defaults=list(), excludeArgs=c()) {
  classRep = getClass(Class)
  assert_that('clMethod' %in% names(classRep@contains), msg='specified class does not inherit from clMethod')
  assert_that(is.call(call))
  assert_that(is.function(defaults) || is.list(defaults) && all(vapply(defaults, is.function, FUN.VALUE=TRUE)))
  assert_that(is.null(excludeArgs) || is.character(excludeArgs))

  if(is.function(defaults)) {
    defaults = list(defaults)
  }

  allArgs = lapply(defaults, formals) %>%
    do.call(c, .) %>%
    as.list()

  # drop arguments without defaults (empty symbols)
  symMask = vapply(allArgs, is.symbol, FUN.VALUE = TRUE)
  dropSymMask = vapply(allArgs[symMask], nchar, FUN.VALUE = 0) == 0
  allArgs[which(symMask)[dropSymMask]] = NULL

  # update arguments
  args = allArgs[not(names(allArgs) %in% excludeArgs)] %>%
    modifyList(as.list(call)[-1], keep.null=TRUE)

  if(any(names(call[-1]) %in% excludeArgs)) {
    warning(sprintf('arguments (%s) cannot be defined for this clMethod class. These arguments will be ignored.', paste0(excludeArgs, collapse=', ')))
  }

  # exclude arguments
  argOrder = union(names(call[-1]), setdiff(names(allArgs), excludeArgs))

  # create call and clMethod
  newCall = do.call('call', c(Class, lapply(args[argOrder], enquote)))
  new(Class, call=newCall)
}

#' @export
is.clMethod = function(object) {
  isS4(object) && is(object, 'clMethod')
}

#' @export
#' @title Check whether the argument of a clMethod has a defined value.
#' @keywords internal
isArgDefined = function(object, name, envir=NULL) {
  envir = clMethod.env(object, parent.frame(), envir)
  assert_that(is.clMethod(object))
  assert_that(is.character(name))

  if(!hasName(object, name)) {
    return(FALSE)
  }
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
setGeneric('getShortName', function(object) standardGeneric('getShortName'))
setMethod('getShortName', signature('clMethod'), getName)


#' @export
#' @title Extract formula
#' @examples
#' m = clMethodKML(Value ~ Time)
#' formula(m) # Value ~ Time
#' @family clMethod functions
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
#' @family clMethod functions
setMethod('names', signature('clMethod'), function(x) {
  names(getCall(x))[-1]
})

#' @title Length of a clMethod object
#' @description Extracts the number of specified arguments of a `clMethod` object.
#' @examples
#' m = clMethodKML()
#' length(m)
#' @family clMethod functions
setMethod('length', signature('clMethod'), function(x) {
  length(getCall(x)) - 1
})


#' @title Retrieve and evaluate a clMethod argument by name
#' @param name Name of the argument to retrieve.
#' @examples
#' m = clMethodKML()
#' m$nClusters
#' @family clMethod functions
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
#' @family clMethod functions
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
    value = tryCatch({
      eval(arg, envir=envir)
    }, error=function(e) {
      # try evaluation within package scope instead
      tryCatch({
        eval(arg, envir=parent.env(getNamespace(.packageName)))
      }, error=function(e) {
          stop(sprintf('error in evaluating clMethod argument "%s" with expression "%s":\n\t%s', i, deparse(e$call), e$message))
        })
    })
  } else {
    value = arg
  }

  if(is.formula(value)) {
    environment(value) = new.env()
  }

  return(value)
})


#' @export
#' @family clMethod functions
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
#' @family clMethod functions
update.clMethod = function(object, ..., envir=NULL) {
  envir = clMethod.env(object, parent.frame(), envir)
  ucall = match.call()[c(-1, -2)]
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
#' @param fun Narrows down the returned arguments to the formal arguments of the given function.
#' @param envir The environment in which to evaluate the arguments.
#' @examples
#' method = clMethodKML()
#' as.list(method)
#' @family clMethod functions
as.list.clMethod = function(object, eval=TRUE, fun=NULL, envir=NULL) {
  envir = clMethod.env(object, parent.frame(), envir)
  if(is.null(fun) || '...' %in% formalArgs(fun)) {
    argNames = names(object)
  } else {
    argNames = intersect(names(object), formalArgs(fun))
  }

  if(isTRUE(eval) || is.scalar(eval) && eval == 'ANY') {
    # full evaluation
    argValues = lapply(argNames, function(argName) object[[argName, envir=envir]])
    names(argValues) = argNames
    return(argValues)
  } else if(is.character(eval)) {
    # partial update
    argValues = as.list(object@call[-1])
    evalValues = vector(mode='list', length=length(object))
    evalMask = vapply(argNames, isArgDefined, object=object, envir=envir, FUN.VALUE=FALSE)
    evalValues[evalMask] = lapply(argNames[evalMask], function(name) object[[name, eval=TRUE, envir=envir]])
    updateMask = evalMask & vapply(evalValues, class, FUN.VALUE='') %in% eval
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
#' @family clMethod functions
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
      deparse(x) %>% paste0(collapse='')
    }
  })

  assert_that(all(vapply(dfList, length, FUN.VALUE=0) == 1))
  as.data.frame(dfList, stringsAsFactors=FALSE)
}

#' @export
#' @title Convert clMethod arguments to character vector
#' @description Converts the arguments of a `clMethod` to a named `character` vector.
#' @inheritParams as.data.frame.clMethod
#' @param nullString Character to use to represent NULL values.
#' @seealso as.data.frame.clMethod
#' @family clMethod functions
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
    } else if(is.character(x)) {
      paste0('"', x, '"', collapse=', ')
    } else if(is.atomic(x)) {
      paste0(as.character(x), collapse=', ')
    } else {
      deparse(x) %>% paste0(collapse='')
    }
  })

  assert_that(all(vapply(chrValues, length, FUN.VALUE=0) == 1))
  unlist(chrValues)
}


#' @title Substitute the call arguments
#' @param envir The environment in which to evaluate the arguments.
#' @return A new call with the substituted arguments.
#' @family clMethod functions
#' @keywords internal
substitute.clMethod = function(object, envir=NULL) {
  envir = clMethod.env(object, parent.frame(), envir)
  assert_that(is.clMethod(object))
  argValues = as.list(object, eval=TRUE, envir=envir)
  object@call = replace(getCall(object), names(argValues), argValues)
  return(object)
}


#' @export
print.clMethod = function(object, ...) {
  argNames = names(object)
  args = as.character(object, ...) %>%
    vapply(strtrim, 40, FUN.VALUE='')

  cat(sprintf(' %-16s%s\n', paste0(argNames, ':'), args), sep='')
}


#' @export
#' @title Generate a list of clMethod
#' @param method The `clMethod` to use as the template, which will be updated for each of the other arguments.
#' @param ... Any other arguments to update the `clMethod` definition with. Values must be `scalar`, `vector`, `list`, or encapsulated in a `.()` call.
#' Arguments wrapped in `.()` are passed as-is to the model call, ensuring a readable method.
#' Arguments comprising a single `symbol` (e.g. a variable name) are interpreted as a constant. To force evaluation, specify `arg=(var)` or `arg=force(var)`.
#' Arguments of type `vector` or `list` are split across a series of method fit calls.
#' Arguments of type `scalar` are constant across the method fits.
#' If a `list` is intended to be passed as a constant argument, then specifying `arg=.(listObject)` results in it being treated as such.
#' @param envir The `environment` in which to evaluate the method arguments.
#' @return A `list` of `clMethod` objects.
#' @examples
#' kml = clMethodKML()
#' methods = clMethods(kml, nClusters=1:6)
#'
#' nclus = 1:6
#' methods = clMethods(kml, nClusters=nclus)
#'
#' methods = clMethods(kml, nClusters=3, center=.(meanNA, meanNA, median))
clMethods = function(method, ..., envir=NULL) {
  assert_that(inherits(method, 'clMethod'), msg='method must be an object of class clMethod')
  envir = clMethod.env(method, parent.frame(), envir)

  mc = match.call()[-1]
  argNames = names(mc) %>% setdiff(c('', 'method', 'envir'))
  argCalls = mc[argNames]

  nameMask = vapply(argCalls, is.name, FUN.VALUE=FALSE)
  dotMask = vapply(argCalls, function(x) is.call(x) && x[[1]] == '.', FUN.VALUE=FALSE)
  evalMask = !nameMask & !dotMask
  evalArgs = lapply(argCalls[evalMask], eval, envir=parent.frame())

  dotLengths = vapply(argCalls[dotMask], length, FUN.VALUE=0) - 1
  evalLengths = lengths(evalArgs)
  nModels = max(1, dotLengths, evalLengths)

  assert_that(all(c(dotLengths, evalLengths) %in% c(1L, nModels)), msg=sprintf('arguments must be of length 1 or of equal length to all other arguments (%d)', nModels))

  nameArgs = lapply(which(nameMask), function(i) as.list(argCalls[[i]]))
  dotArgs = lapply(which(dotMask), function(i) as.list(argCalls[[i]][-1]))

  firstOrN = function(x, i) x[[min(length(x), i)]]

  # using mapply results in dots[[1L]] errors
  methods = vector('list', nModels)
  for(i in seq_len(nModels)) {
    methods[[i]] = do.call(update,
                           c(object=method,
                             lapply(nameArgs, firstOrN, i),
                             lapply(dotArgs, firstOrN, i),
                             lapply(evalArgs, firstOrN, i),
                             envir=envir))
  }

  assert_that(all(vapply(nameArgs, is.list, FUN.VALUE=FALSE)),
              all(vapply(dotArgs, is.list, FUN.VALUE=FALSE)),
              all(vapply(evalArgs, is.vector, FUN.VALUE=FALSE)), msg='The processed argument lists are in an unexpected format. Please report this issue.')

  return(methods)
}

# Local methods ####
# . prepare ####
#' @title clMethod interface function
#' @description Called by [cluslong].
setGeneric('prepare', function(method, ...) standardGeneric('prepare'))
setMethod('prepare', signature('clMethod'), function(method, data, verbose) {})

# . fit ####
#' @title clMethod interface function
#' @description Called by [cluslong].
setGeneric('fit', function(method, ...) standardGeneric('fit'))

#' @title Select the preferred environment
#' @description Returns envir if specified. Otherwise, returns environment(object) if specified. The defaultEnvir is returned when the former two are NULL.
#' @keywords internal
clMethod.env = function(object, defaultEnvir, envir) {
  assert_that(is.clMethod(object))
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
