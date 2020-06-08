# Method ####
#' @export
#' @name clMethod-class
#' @title clMethod class
#' @description Base class used to define a longitudinal cluster method. It is implemented as a wrapper around a `call`.
#' @details Because the `clMethod` arguments may be unevaluated, evaluation functions such as `[[` accept an `envir` argument.
#' A default `environment` can be assigned or obtained from a `clMethod` object using the `environment()` function.
#' @seealso \link{environment}
#' @slot call The `call` representing the arguments of the `clMethod` object.
#' @family clMethod implementations
setClass('clMethod', slots=c(call='call'))

#. initialize ####
setMethod('initialize', 'clMethod', function(.Object, ...) {
  .Object = callNextMethod()
  validObject(.Object)
  .Object
})

#. validity ####
setValidity('clMethod', function(object) {
  call = getCall(object)
  assert_that(all(vapply(lapply(names(object), nchar), '>', 0, FUN.VALUE=TRUE)), msg='clMethod argument names cannot be empty')
  assert_that(!any(vapply(names(object), startsWith, '.', FUN.VALUE=TRUE)), msg='clMethod argument names cannot start with "."')

  if(isArgDefined(object, 'formula')) {
    assert_that(is.formula(object$formula))
  }

  if(isArgDefined(object, 'nClusters')) {
    assert_that(is.na(object$nClusters) || is.count(object$nClusters))
  }
})

#. $ ####
#' @name $,clMethod
#' @title Retrieve and evaluate a clMethod argument by name
#' @param name Name of the argument to retrieve.
#' @examples
#' m = clMethodKML()
#' m$nClusters
#' @family clMethod functions
setMethod('$', signature('clMethod'), function(x, name) {
  x[[name]]
})


#. [[ ####
#' @name [[,clMethod
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
#' m[['nClusters']] # 2
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
#' @title Construct a clMethod object for a given method type
#' @description Creates a clMethod class of the specified type `Class` for the given arguments given in a call, along with any default arguments from reference functions.
#' This function is intended to be used by classes extending `clMethod` to provide an easy way to construct the appropriate `call` object.
#' @param Class The type of \link{clMethod} class
#' @param call The arguments to create the `clMethod` from.
#' @param defaults List of `function` to obtain defaults from for arguments not defined in `call`.
#' @param excludeArgs The names of the arguments to exclude from the defaults, provided as a `character vector`.
#' @examples
#' clMethodKML2 = function(formula=Value ~ 0, time='Id', id='Id', nClusters=2, ...) {
#'   .clMethod('clMethodKML', call=stackoverflow::match.call.defaults(),
#'     defaults=c(kml::kml, kml::parALGO),
#'     excludeArgs=c('object', 'nbClusters', 'parAlgo', 'toPlot', 'saveFreq'))
#' }
#' m = clMethodKML2(nClusters=3)
#' cluslong(m, testLongData)
#' @family clMethod functions
.clMethod = function(Class, call, defaults=list(), excludeArgs=c()) {
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
#' @title Extract the method arguments as a list
#' @param args A `character vector` of argument names to select. Only available arguments are returned.
#' Alternatively, a `function` or `list` of `function`s, whose formal arguments will be selected from the method.
#' @param eval Whether to evaluate the arguments.
#' @param expand Whether to return all method arguments when `"..."` is present among the requested argument names.
#' @param envir The `environment` in which to evaluate the arguments. If `NULL`, the environment associated with the object is used. If not available, the `parent.frame()` is used.
#' @examples
#' method = clMethodKML()
#' as.list(method)
#'
#' as.list(method, args=c('id', 'time'))
#'
#' as.list(method, args=kml::kml)
#'
#' as.list(method, args=c(kml::kml, kml::parALGO))
#' @family clMethod functions
as.list.clMethod = function(object, args=names(object), eval=TRUE, expand=FALSE, envir=NULL) {
  assert_that(is.clMethod(object),
              is.flag(eval),
              is.flag(expand))
  envir = clMethod.env(object, parent.frame(), envir)

  if(is.function(args)) {
    argNames = formalArgs(args)
  }
  else if(is.list(args)) {
    # functions special case
    argNames = lapply(args, formalArgs) %>%
      Reduce(union, .)
  } else {
    assert_that(is.character(args))
    argNames = args
  }

  # filter arguments
  if(isTRUE(expand) && '...' %in% argNames) {
    selArgNames = argNames
  } else {
    selArgNames = intersect(argNames, names(object))
  }

  if(isTRUE(eval)) {
    # full evaluation
    method = substitute.clMethod(object, envir=envir)
  } else {
    method = object
  }

  as.list(method@call[-1][selArgNames])
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
                                  nullValue=NA,
                                  envir=NULL) {
  assert_that(is.clMethod(x),
              is.flag(eval),
              length(nullValue) == 1)

  if(isTRUE(eval)) {
    envir = clMethod.env(x, parent.frame(), envir)
    evalClasses = c('NULL', 'logical', 'numeric', 'complex', 'integer', 'character', 'factor')
    method = substitute.clMethod(x, classes = evalClasses, envir=envir)
  } else {
    method = x
  }
  argList = as.list(method, eval=FALSE)

  dfList = lapply(argList, function(a) {
    if(is.null(a)) {
      nullValue
    } else if(is.atomic(a)) {
      if(length(a) > 1) {
        dput(a)
      } else {
        a
      }
    } else {
      deparse(a) %>% paste0(collapse='')
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
                                 nullString='NULL',
                                 envir=NULL) {
  assert_that(is.clMethod(x),
              is.flag(eval),
              is.character(nullString),
              is.scalar(nullString))

  if(isTRUE(eval)) {
    envir = clMethod.env(x, parent.frame(), envir)
    evalClasses = c('NULL', 'logical', 'numeric', 'complex', 'integer', 'character', 'factor')
    method = substitute.clMethod(x, classes = evalClasses, envir=envir)
  } else {
    method = x
  }
  argList = as.list(method, eval=FALSE)

  chrValues = lapply(argList, function(a) {
    if(is.null(a)) {
      nullString
    } else if(is.character(a)) {
      paste0('"', a, '"', collapse=', ')
    } else if(is.atomic(a)) {
      paste0(as.character(a), collapse=', ')
    } else {
      deparse(a) %>% paste0(collapse='')
    }
  })

  assert_that(all(vapply(chrValues, length, FUN.VALUE=0) == 1))
  unlist(chrValues)
}



#' @export
#' @family clMethod functions
setGeneric('getName', function(object) standardGeneric('getName'))
setMethod('getName', signature('clMethod'), function(object) 'custom')

#' @export
#' @family clMethod functions
setGeneric('getShortName', function(object) standardGeneric('getShortName'))
setMethod('getShortName', signature('clMethod'), getName)


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


#' @export
#' @title Generate a list of clMethod objects
#' @description Generates a list of `clMethod` objects for a sequence of argument values.
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
  assert_that(is.clMethod(method))

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



# . fit ####
#' @export
#' @title clMethod interface function
#' @description Called by [cluslong].
setGeneric('fit', function(method, ...) standardGeneric('fit'))




#' @export
#' @title Extract formula
#' @examples
#' m = clMethodKML(Value ~ Time)
#' formula(m) # Value ~ Time
#' @family clMethod functions
formula.clMethod = function(object, what='mu', envir=NULL) {
  assert_that(is.clMethod(object))
  envir = clMethod.env(object, parent.frame(), envir)
  assert_that(is.scalar(what), is.character(what))
  if (what == 'mu') {
    object$formula
  } else {
    object[[paste0('formula.', what)]]
  }
}



#' @export
#' @family clMethod functions
getCall.clMethod = function(object) {
  assert_that(is.clMethod(object))
  object@call
}


#' @export
#' @title Check whether the argument of a clMethod has a defined value.
#' @param name The name of the argument.
#' @param envir The `environment` to evaluate the arguments in. If `NULL`, the argument is not evaluated.
#' @keywords internal
isArgDefined = function(object, name, envir=environment(object)) {
  assert_that(is.clMethod(object))
  assert_that(is.character(name), is.scalar(name))
  assert_that(is.environment(envir) || is.null(envir))

  if(!hasName(object, name)) {
    return(FALSE)
  }
  arg = object[[name[1], eval=FALSE]]


  if(is.name(arg) || is.call(arg)) {
    if(is.null(envir)) {
      return(FALSE)
    } else {
      arg = try(object[[name, envir=envir]], silent=TRUE)
      return(!is(arg, 'try-error'))
    }
  } else {
    return(TRUE)
  }
}


#' @export
is.clMethod = function(object) {
  isS4(object) && is(object, 'clMethod')
}


#. length ####
#' @title Length of a clMethod object
#' @description Extracts the number of specified arguments of a `clMethod` object.
#' @examples
#' m = clMethodKML()
#' length(m)
#' @family clMethod functions
setMethod('length', signature('clMethod'), function(x) {
  length(getCall(x)) - 1
})



#. names ####
#' @title clMethod argument names
#' @examples
#' m = clMethodKML()
#' names(m)
#' @family clMethod functions
setMethod('names', signature('clMethod'), function(x) {
  argNames = names(getCall(x))[-1]
  if(is.null(argNames)) {
    character(0)
  } else {
    argNames
  }
})



#' @export
print.clMethod = function(object, ..., width=40) {
  assert_that(is.clMethod(object))

  argNames = names(object)
  args = as.character(object, ...) %>%
    vapply(strtrim, width=width, FUN.VALUE='')

  if(length(args) > 0) {
    cat(sprintf(' %-16s%s\n', paste0(argNames, ':'), args), sep='')
  } else {
    cat(' no arguments\n')
  }
}

#' @title Substitute the call arguments for their evaluated values
#' @description Substitutes call arguments if they can be evaluated without error.
#' @inheritParams as.list.clMethod
#' @param classes Substitute only arguments with specific class types. By default, all types are substituted.
#' @return A new call with the substituted arguments.
#' @family clMethod functions
#' @keywords internal
substitute.clMethod = function(object, classes='ANY', envir=NULL) {
  assert_that(is.clMethod(object))
  assert_that(is.character(classes))

  envir = clMethod.env(object, parent.frame(), envir)

  argNames = names(object)
  argValues = as.list(object@call[-1])
  evalMask = vapply(argNames, isArgDefined, object=object, envir=envir, FUN.VALUE=FALSE)
  evalValues = vector(mode='list', length=length(object))
  evalValues[evalMask] = lapply(argNames[evalMask], function(name) object[[name, eval=TRUE, envir=envir]])

  if('ANY' %in% classes) {
    updateMask = evalMask
  } else {
    updateMask = evalMask & vapply(evalValues, class, FUN.VALUE='') %in% classes
  }

  newmethod = object
  newmethod@call = replace(getCall(object), names(object)[updateMask], evalValues[updateMask])
  return(newmethod)
}



#' @export
#' @title Update a method specification
#' @details Updates or adds arguments to a `clMethod` object. The inputs are evaluated in order to determine the presence of `formula` objects, which are updated accordingly.
#' @inheritParams as.list.clMethod
#' @param .eval Whether to assign the evaluated argument values to the method. By default (`FALSE`), the argument expression is preserved.
#' @return The new `clMethod` object with the additional or updated arguments.
#' @examples
#' m = clMethodKML(Value ~ 1)
#' m2 = update(m, formula=~ . + Time)
#'
#' m3 = update(m2, start='randomAll')
#'
#' xvar = 2
#' m4 = update(m, x=xvar) # x: xvar
#'
#' m5 = update(m, x=xvar, .eval=TRUE) # x: 2
#'
#' @family clMethod functions
update.clMethod = function(object, ..., .eval=FALSE, envir=NULL) {
  assert_that(is.clMethod(object),
              is.flag(.eval))

  envir = clMethod.env(object, parent.frame(), envir)

  argNames = names(object)
  if(isTRUE(.eval)) {
    ucall = list(...)
    uargValues = ucall
  } else {
    ucall = match.call()[c(-1, -2)]
    ucall$envir = NULL
    ucall$.eval = NULL
    uargValues = lapply(ucall, eval, envir=envir)
  }
  uargNames = names(ucall)

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







# . prepare ####
#' @export
#' @title clMethod interface function
#' @description Called by [cluslong].
setGeneric('prepare', function(method, ...) standardGeneric('prepare'))
setMethod('prepare', signature('clMethod'), function(method, data, verbose) {})



#. show ####
setMethod('show', 'clMethod',
          function(object) {
            cat(class(object)[1], ' as "', getName(object), '"\n', sep='')
            print(object)
          }
)



