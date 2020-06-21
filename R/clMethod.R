# Method ####
#' @export
#' @name clMethod-class
#' @title clMethod class
#' @description Base class used to define a longitudinal cluster method. It is implemented as a wrapper around a `call`.
#' @details Because the `clMethod` arguments may be unevaluated, evaluation functions such as `[[` accept an `envir` argument.
#' A default `environment` can be assigned or obtained from a `clMethod` object using the `environment()` function.
#' @seealso \link{environment}
#' @slot arguments A `list` representing the arguments of the `clMethod` object. Arguments are not evaluated upon creation of the method object. Instead, arguments are stored similar to a `call` object. Do not modify or access.
#' @slot sourceCalls A list of calls for tracking the original call after substitution. Used for printing objects which require too many characters (e.g. ,function definitions, matrices).
#' @family clMethod implementations
setClass('clMethod', slots=c(arguments='list', sourceCalls='list'))

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
  assert_that(!has_name(object, 'data'), msg='clMethod argument name cannot be "data"')
  assert_that(!has_name(object, 'envir'), msg='clMethod argument name cannot be "envir"')
  assert_that(!has_name(object, 'verbose'), msg='clMethod argument name cannot be "verbose"')

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
#' @return The argument evaluation result.
#' @examples
#' m = clMethodKML(nClusters = 3)
#' m$nClusters # 3
#' @family clMethod functions
setMethod('$', signature('clMethod'), function(x, name) {
  x[[name]]
})


#. [[ ####
#' @name [[,clMethod
#' @title Retrieve and evaluate a clMethod argument by name
#' @param i Name or index of the argument to retrieve.
#' @param eval Whether to evaluate the call argument (enabled by default).
#' @param envir The `environment` in which to evaluate the argument. This argument is only applicable when `eval = TRUE`.
#' @return The argument `call` or evaluation result.
#' @examples
#' m = clMethodKML(nClusters = 5)
#' m[['nClusters']] # 5
#'
#' k = 2
#' m = clMethodKML(nClusters = k)
#' m[['nClusters']] # 2
#' m[['nClusters', eval=FALSE]] # k
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
#' @title Create a clMethod object for an arbitrary class
#' @description Provides a mechanism for creating `clMethod` objects for an arbitrary class.
#' Note that it is advisable to use the class-specific constructors instead.
#' @param Class The type of \link{clMethod} class
#' @param ... Any arguments to assign to the method object.
.clMethod = function(.class, ..., .defaults=list(), .excludeArgs=c()) {
  args = list(...)
  do.call(.clMethod.call, list(.class = .class, args, .defaults = .defaults, .excludeArgs = .excludeArgs))
}

#' @export
#' @title Create a clMethod object from a call
#' @description Creates a clMethod class of the specified type `Class` for the given arguments given in a call, along with any default arguments from reference functions.
#' This function is intended to be used by classes extending `clMethod` to provide an easy way to construct the appropriate `call` object.
#' @param Class The type of \link{clMethod} class
#' @param call The arguments to create the `clMethod` from.
#' @param defaults List of `function` to obtain defaults from for arguments not defined in `call`.
#' @param excludeArgs The names of the arguments to exclude from the defaults, provided as a `character vector`.
#' @return An object of class `Class` that extends `clMethod`.
#' @examples
#' clMethodKML2 = function(formula=Value ~ 0, time='Id', id='Id', nClusters=2, ...) {
#'   .clMethod.call('clMethodKML', call=stackoverflow::match.call.defaults(),
#'     defaults=c(kml::kml, kml::parALGO),
#'     excludeArgs=c('object', 'nbClusters', 'parAlgo', 'toPlot', 'saveFreq'))
#' }
#' m = clMethodKML2(nClusters=3)
#' cluslong(m, testLongData)
#' @family clMethod functions
.clMethod.call = function(Class, call, defaults=list(), excludeArgs=c()) {
  classRep = getClass(Class)
  assert_that('clMethod' %in% names(classRep@contains), msg='specified class does not inherit from clMethod')
  assert_that(is.call(call))
  assert_that(is.function(defaults) || is.list(defaults) && all(vapply(defaults, is.function, FUN.VALUE=TRUE)))
  assert_that(is.null(excludeArgs) || is.character(excludeArgs))

  excludeArgs = union(excludeArgs, c('verbose', 'envir', 'data'))

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
    warning(sprintf('arguments (%s) cannot be defined for this clMethod class. These arguments will be ignored.', paste0(intersect(excludeArgs, names(call[-1])), collapse=', ')))
  }

  # exclude arguments
  argOrder = union(names(call[-1]), setdiff(names(allArgs), excludeArgs))

  # create call and clMethod
  newCall = do.call('call', c(Class, lapply(args[argOrder], enquote)))
  new(Class, arguments=as.list(newCall[-1]))
}


#' @export
#' @title Extract the method arguments as a list
#' @param args A `character vector` of argument names to select. Only available arguments are returned.
#' Alternatively, a `function` or `list` of `function`s, whose formal arguments will be selected from the method.
#' @param eval Whether to evaluate the arguments.
#' @param expand Whether to return all method arguments when `"..."` is present among the requested argument names.
#' @param envir The `environment` in which to evaluate the arguments. If `NULL`, the environment associated with the object is used. If not available, the `parent.frame()` is used.
#' @return A `list` with the argument `call`s or evaluated results depending on the value for `eval`.
#' @examples
#' method = clMethodKML()
#' as.list(method)
#'
#' as.list(method, args=c('id', 'time'))
#'
#' # select arguments used by kml()
#' as.list(method, args=kml::kml)
#'
#' # select arguments used by either kml() or parALGO()
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

  as.list(method@arguments[selArgNames])
}


#' @export
#' @title Convert clMethod arguments to a list of atomic types
#' @description Converts the arguments of a `clMethod` to a named `list` of [atomic] types.
#' @inheritParams as.list.clMethod
#' @param x `clMethod` to be coerced to a `character` `vector`.
#' @param eval Whether to evaluate the arguments in order to replace expression if the resulting value is of a class specified in `evalClasses`.
#' @param nullValue Value to use to represent the `NULL` type. Must be of length 1.
#' @return A single-row `data.frame` where each columns represents an argument call or evaluation.
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


#. compose ####
#' @export
setGeneric('compose', function(method, ...) standardGeneric('compose'))
#' @export
#' @rdname clMethod-interface
setMethod('compose', signature('clMethod'), function(method, envir=NULL) {
  substitute.clMethod(method, try=FALSE, envir=envir)
})


# . fit ####
#' @export
setGeneric('fit', function(method, ...) standardGeneric('fit'))
#' @export
#' @rdname clMethod-interface
#' @title clMethod interface
#' @description Called by [cluslong].
#' * compose
#' * validate
#' * prepareData
#' * preFit
#'
#' derpy
#' * fit
#' * postFit
setMethod('fit', signature('clMethod'), function(method, data, envir, verbose) {
  stop(sprintf('method cannot be estimated because the fit() function is not implemented for clMethod of class %s.
   define the fit() method using:
      \tsetMethod("fit", signature("%s"), function(method, data, verbose) {
      \t\t<your code returning a clModel-extended class here>
      \t})")'), class(method)[1], class(method)[1])
})


#' @export
#' @title Extract formula
#' @description Extracts the associated `formula` for the given distributional parameter.
#' @inheritParams as.list.clMethod
#' @param what The distributional parameter to which this formula applies. By default, the formula specifies `"mu"`.
#' @return The `formula` for the given distributional parameter.
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
  do.call(call, c(class(object)[1], lapply(object@arguments, enquote)))
}


#. getLabel ####
#' @export
setGeneric('getLabel', function(object, ...) standardGeneric('getLabel'))
#' @export
#' @rdname clMethod-interface
setMethod('getLabel', signature('clMethod'), function(object) {
  if(hasName(object, 'label')) {
    object$label
  } else {
    ''
  }
})


#. getName ####
#' @export
setGeneric('getName', function(object, ...) standardGeneric('getName'))

#' @export
#' @rdname clMethod-interface
setMethod('getName', signature('clMethod'), function(object) 'custom')

#. getShortName ####
#' @export
setGeneric('getShortName', function(object, ...) standardGeneric('getShortName'))
#' @export
#' @rdname clMethod-interface
setMethod('getShortName', signature('clMethod'), getName)


#. idVariable ####
#' @export
setGeneric('idVariable', function(object, ...) standardGeneric('idVariable'))
#' @export
setMethod('idVariable', signature('clMethod'), function(object) object$id)


#' @export
#' @title Check whether the argument of a clMethod has a defined value.
#' @description Determines whether the associated argument value is defined. If the argument value is of type `language`, the argument is evaluated to see if it can be resolved within its `environment`.
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


  if(is.language(arg)) {
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
#' @return The number of specified arguments.
#' @examples
#' m = clMethodKML()
#' length(m)
#' @family clMethod functions
setMethod('length', signature('clMethod'), function(x) {
  length(getCall(x)) - 1
})



#. names ####
#' @title clMethod argument names
#' @return A `character vector` of argument names.
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


# . preFit ####
#' @export
setGeneric('preFit', function(method, ...) standardGeneric('preFit'))
#' @rdname clMethod-interface
setMethod('preFit', signature('clMethod'), function(method, data, envir, verbose) {
  return(envir)
})


# . prepareData ####
#' @export
setGeneric('prepareData', function(method, ...) standardGeneric('prepareData'))
#' @rdname clMethod-interface
setMethod('prepareData', signature('clMethod'), function(method, data, verbose) {
  return(NULL)
})


# . postfit ####
#' @export
setGeneric('postFit', function(method, ...) standardGeneric('postFit'))
#' @rdname clMethod-interface
setMethod('postFit', signature('clMethod'), function(method, data, model, envir, verbose) {
  return(model)
})


#' @export
print.clMethod = function(object, ..., eval=FALSE, width=40, envir=NULL) {
  assert_that(is.clMethod(object),
              is.flag(eval))
  envir = clMethod.env(object, parent.frame(), envir)
  if(isTRUE(eval)) {
    object = substitute.clMethod(object, envir=envir)
  }

  arg2char = function(a) {
    if(is.null(a)) {
      'NULL'
    } else if(is.character(a)) {
      paste0('"', a, '"', collapse=', ')
    } else if(is.atomic(a)) {
      paste0(as.character(a), collapse=', ')
    } else {
      deparse(a) %>% paste0(collapse='')
    }
  }

  argNames = names(object)
  chrValues = lapply(object@arguments, arg2char) %>% unlist()
  assert_that(all(vapply(chrValues, length, FUN.VALUE=0) == 1))

  sourceMask = vapply(chrValues, nchar, FUN.VALUE=0) > width & argNames %in% names(object@sourceCalls)
  chrSource = lapply(object@sourceCalls[argNames[sourceMask]], arg2char) %>% unlist()
  chrValues[sourceMask] = paste('<eval>', chrSource)

  args = vapply(chrValues, strtrim, width=width, FUN.VALUE='')

  if(length(args) > 0) {
    cat(sprintf(' %-16s%s\n', paste0(argNames, ':'), args), sep='')
  } else {
    cat(' no arguments\n')
  }
}

#' @title Substitute the call arguments for their evaluated values
#' @description Substitutes the call arguments if they can be evaluated without error.
#' @inheritParams as.list.clMethod
#' @param classes Substitute only arguments with specific class types. By default, all types are substituted.
#' @param try Whether to try to evaluate arguments and ignore errors (the default), or to fail on any argument evaluation error.
#' @param exclude Arguments to exclude from evaluation.
#' @return A new `clMethod` object with the substituted arguments.
#' @family clMethod functions
#' @keywords internal
substitute.clMethod = function(object, classes='ANY', try=TRUE, exclude=character(), envir=NULL) {
  assert_that(is.clMethod(object))
  assert_that(is.character(classes))

  envir = clMethod.env(object, parent.frame(), envir)

  argNames = names(object)
  if(isTRUE(try)) {
    evalMask = vapply(argNames, isArgDefined, object=object, envir=envir, FUN.VALUE=FALSE) & !(argNames %in% exclude)
  } else {
    evalMask = !(argNames %in% exclude)
  }

  evalValues = vector(mode='list', length=length(object))
  evalValues[evalMask] = lapply(argNames[evalMask], function(name) object[[name, eval=TRUE, envir=envir]])

  if('ANY' %in% classes) {
    updateMask = evalMask
  } else {
    updateMask = evalMask & vapply(evalValues, class, FUN.VALUE='') %in% classes
  }

  newmethod = object
  sourceMask = vapply(newmethod@arguments, is.language, FUN.VALUE=FALSE)
  newmethod@sourceCalls[argNames[updateMask & sourceMask]] = newmethod@arguments[updateMask & sourceMask]
  newmethod@arguments = replace(object@arguments, names(object)[updateMask], evalValues[updateMask])
  return(newmethod)
}



#' @export
#' @title Update a method specification
#' @details Updates or adds arguments to a `clMethod` object. The inputs are evaluated in order to determine the presence of `formula` objects, which are updated accordingly.
#' @inheritParams as.list.clMethod
#' @param .eval Whether to assign the evaluated argument values to the method. By default (`FALSE`), the argument expression is preserved.
#' @param .remove Names of arguments that should be removed.
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
update.clMethod = function(object, ..., .eval=FALSE, .remove=character(), envir=NULL) {
  assert_that(is.clMethod(object),
              is.flag(.eval),
              is.character(.remove))

  envir = clMethod.env(object, parent.frame(), envir)

  argNames = names(object)
  if(isTRUE(.eval)) {
    ucall = list(...)
    uargValues = ucall
  } else {
    ucall = match.call()[c(-1, -2)]
    ucall$envir = NULL
    ucall$.eval = NULL
    ucall$.remove = NULL
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

  object@arguments = replace(object@arguments, uargNames, ucall[uargNames])
  object@sourceCalls[uargNames] = NULL

  if(length(.remove) > 0) {
    object@arguments[.remove] = NULL
    object@sourceCalls[.remove] = NULL
  }
  validObject(object)
  return(object)
}



#. responseVariable ####
#' @export
setGeneric('responseVariable', function(object, ...) standardGeneric('responseVariable'))
#' @export
#' @title Determine the response variable
#' @return The response variable
setMethod('responseVariable', signature('clMethod'), function(object) {
  if(hasName(object, 'response')) {
    object$response
  } else if(hasName(object, 'formula')) {
    getResponse(object$formula)
  } else {
    stop('cannot determine the response variable(s) for class ', class(object)[1],
         'Consider overriding "responseVariable(clMethod)" to fix this for your clMethod implementation')
  }
})


#. show ####
setMethod('show', 'clMethod', function(object) {
  cat(class(object)[1], ' as "', getName(object), '"\n', sep='')
  print(object)
})


#. timeVariable ####
#' @export
setGeneric('timeVariable', function(object, ...) standardGeneric('timeVariable'))
#' @export
setMethod('timeVariable', signature('clMethod'), function(object) object$time)


#. validate ####
#' @export
setGeneric('validate', function(method, data, ...) standardGeneric('validate'))
#' @export
#' @rdname clMethod-interface
setMethod('validate', signature('clMethod'), function(method, data, envir=NULL) {
  validate_that(hasName(data, idVariable(method)),
                hasName(data, timeVariable(method)),
                hasName(data, responseVariable(method)),
                is.character(getLabel(method)))
})


