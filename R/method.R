# Method ####
#' @export
#' @name lcMethod-class
#' @title lcMethod class
#' @description Base class used to define a longitudinal cluster method. It is implemented as a wrapper around a `call`.
#' @details Because the `lcMethod` arguments may be unevaluated, evaluation functions such as `[[` accept an `envir` argument.
#' A default `environment` can be assigned or obtained from a `lcMethod` object using the `environment()` function.
#' @seealso \link{environment}
#' @slot arguments A `list` representing the arguments of the `lcMethod` object. Arguments are not evaluated upon creation of the method object. Instead, arguments are stored similar to a `call` object. Do not modify or access.
#' @slot sourceCalls A list of calls for tracking the original call after substitution. Used for printing objects which require too many characters (e.g. ,function definitions, matrices).
#' @family lcMethod implementations
setClass('lcMethod', slots = c(arguments = 'environment', sourceCalls = 'list'))

#. initialize ####
setMethod('initialize', 'lcMethod', function(.Object, ...) {
  .Object = callNextMethod()
  validObject(.Object)
  .Object
})

#. validity ####
setValidity('lcMethod', function(object) {
  assert_that(all(vapply(
    lapply(names(object), nchar), '>', 0, FUN.VALUE = TRUE
  )), msg = 'lcMethod argument names cannot be empty')
  assert_that(!any(vapply(
    names(object), startsWith, '.', FUN.VALUE = TRUE
  )), msg = 'lcMethod argument names cannot start with "."')
  assert_that(!has_name(object, 'data'), msg = 'lcMethod argument name cannot be "data"')
  assert_that(!has_name(object, 'envir'), msg = 'lcMethod argument name cannot be "envir"')
  assert_that(!has_name(object, 'verbose'), msg = 'lcMethod argument name cannot be "verbose"')

  if (isArgDefined(object, 'formula')) {
    assert_that(is.formula(object$formula))
  }

  if (isArgDefined(object, 'nClusters')) {
    assert_that(is.na(object$nClusters) || is.count(object$nClusters))
  }
})

#. $ ####
#' @name $,lcMethod
#' @title Retrieve and evaluate a lcMethod argument by name
#' @param name Name of the argument to retrieve.
#' @return The argument evaluation result.
#' @examples
#' m = lcMethodKML(nClusters = 3)
#' m$nClusters # 3
#' @family lcMethod functions
setMethod('$', signature('lcMethod'), function(x, name) {
  x[[name]]
})


#. [[ ####
#' @name [[,lcMethod
#' @title Retrieve and evaluate a lcMethod argument by name
#' @param i Name or index of the argument to retrieve.
#' @param eval Whether to evaluate the call argument (enabled by default).
#' @param envir The `environment` in which to evaluate the argument. This argument is only applicable when `eval = TRUE`.
#' @return The argument `call` or evaluation result.
#' @examples
#' m = lcMethodKML(nClusters = 5)
#' m[['nClusters']] # 5
#'
#' k = 2
#' m = lcMethodKML(nClusters = k)
#' m[['nClusters']] # 2
#' m[['nClusters', eval=FALSE]] # k
#' @family lcMethod functions
setMethod('[[', signature('lcMethod'), function(x, i, eval = TRUE, envir = NULL) {
  envir = lcMethod.env(x, parent.frame(3), envir)
  if (is.character(i)) {
    assert_that(has_name(x, i),
                msg = sprintf('method does not have an argument named "%s"', i))
    arg = get(i, envir = x@arguments)
  } else {
    argName = names(x)[i]
    assert_that(!is.na(argName),
                msg = sprintf('index "%s" exceeded argument name options', i))
    arg = get(i, envir = x@arguments)
  }

  if (eval) {
    # within-method scope
    value = tryCatch({
      eval(arg, envir = x@arguments)
    }, error = function(e) {
      tryCatch({
        eval(arg, envir = envir)
      }, error = function(e2) {
        # try evaluation within package scope instead
        tryCatch({
          eval(arg, envir = parent.env(getNamespace(.packageName)))
        }, error = function(e3) {
          stop(
            sprintf(
              'error in evaluating lcMethod argument "%s" with expression "%s":\n\t%s',
              i,
              deparse(e2$call),
              e2$message
            )
          )
        })
      })
    })
  } else {
    value = arg
  }

  if (is.formula(value)) {
    environment(value) = new.env()
  }

  return(value)
})

#' @export
#' @title Create a lcMethod object of the specified type and arguments
#' @description Provides a mechanism for creating `lcMethod` objects for an arbitrary class.
#' Note that it is advisable to use the class-specific constructors instead.
#' @param Class The type of \link{lcMethod} class
#' @param ... Any arguments to assign to the method object.
#' @seealso [lcMethod.call]
lcMethod = function(.class,
                     ...,
                     .defaults = list(),
                     .excludeArgs = c()) {
  mc = match.call()
  mc[[1]] = as.name(.class)
  mc$.class = NULL
  mc$.defaults = NULL
  mc$.excludeArgs = NULL

  do.call(
    lcMethod.call,
    list(
      Class = .class,
      call = quote(mc),
      defaults = .defaults,
      excludeArgs = .excludeArgs
    )
  )
}

#' @export
#' @title Create a lcMethod object from a call
#' @description Creates a lcMethod class of the specified type `Class` for the given arguments given in a call, along with any default arguments from reference functions.
#' This function is intended to be used by classes extending `lcMethod` to provide an easy way to construct the appropriate `call` object.
#' @param Class The type of \link{lcMethod} class
#' @param call The arguments to create the `lcMethod` from.
#' @param defaults List of `function` to obtain defaults from for arguments not defined in `call`.
#' @param excludeArgs The names of the arguments to exclude from the defaults, provided as a `character vector`.
#' @return An object of class `Class` that extends `lcMethod`.
#' @examples
#' lcMethodKML2 = function(formula=Value ~ 0, time='Id', id='Id', nClusters=2, ...) {
#'   lcMethod.call('lcMethodKML', call=stackoverflow::match.call.defaults(),
#'     defaults=c(kml::kml, kml::parALGO),
#'     excludeArgs=c('object', 'nbClusters', 'parAlgo', 'toPlot', 'saveFreq'))
#' }
#' m = lcMethodKML2(nClusters=3)
#' latrend(m, testLongData)
#' @seealso [lcMethod]
lcMethod.call = function(Class,
                          call,
                          defaults = list(),
                          excludeArgs = c()) {
  classRep = getClass(Class)
  assert_that('lcMethod' %in% names(classRep@contains), msg = 'specified class does not inherit from lcMethod')
  assert_that(is.call(call))
  assert_that(is.function(defaults) ||
                is.list(defaults) &&
                all(vapply(defaults, is.function, FUN.VALUE = TRUE)))
  assert_that(is.null(excludeArgs) || is.character(excludeArgs))

  excludeArgs = union(excludeArgs, c('verbose', 'envir', 'data'))

  if (is.function(defaults)) {
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
    modifyList(as.list(call)[-1], keep.null = TRUE)

  if (any(names(call[-1]) %in% excludeArgs)) {
    warning(
      sprintf(
        'arguments (%s) cannot be defined for this lcMethod class. These arguments will be ignored.',
        paste0(intersect(excludeArgs, names(call[-1])), collapse = ', ')
      )
    )
  }

  # exclude arguments
  argOrder = union(names(call[-1]), setdiff(names(allArgs), excludeArgs))

  argEnv = list2env(rev(args), hash = FALSE)

  new(Class, arguments = argEnv)
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
#' method = lcMethodKML()
#' as.list(method)
#'
#' as.list(method, args=c('id', 'time'))
#'
#' # select arguments used by kml()
#' as.list(method, args=kml::kml)
#'
#' # select arguments used by either kml() or parALGO()
#' as.list(method, args=c(kml::kml, kml::parALGO))
#' @family lcMethod functions
as.list.lcMethod = function(x, ...,
                            args = names(x),
                            eval = TRUE,
                            expand = FALSE,
                            envir = NULL) {
  assert_that(is.lcMethod(x),
              is.flag(eval),
              is.flag(expand))
  envir = lcMethod.env(x, parent.frame(), envir)

  if (is.function(args)) {
    argNames = formalArgs(args)
  }
  else if (is.list(args)) {
    # functions special case
    argNames = lapply(args, formalArgs) %>%
      Reduce(union, .)
  } else {
    assert_that(is.character(args))
    argNames = args
  }

  # filter arguments
  if (isTRUE(expand) && '...' %in% argNames) {
    selArgNames = argNames
  } else {
    selArgNames = intersect(argNames, names(x))
  }

  if (isTRUE(eval)) {
    # full evaluation
    method = evaluate.lcMethod(x, envir = envir)
  } else {
    method = x
  }

  as.list(method@arguments)[selArgNames]
}


#' @export
#' @title Convert lcMethod arguments to a list of atomic types
#' @description Converts the arguments of a `lcMethod` to a named `list` of [atomic] types.
#' @inheritParams as.list.lcMethod
#' @param x `lcMethod` to be coerced to a `character` `vector`.
#' @param eval Whether to evaluate the arguments in order to replace expression if the resulting value is of a class specified in `evalClasses`.
#' @param nullValue Value to use to represent the `NULL` type. Must be of length 1.
#' @return A single-row `data.frame` where each columns represents an argument call or evaluation.
#' @family lcMethod functions
as.data.frame.lcMethod = function(x, ...,
                                  eval = FALSE,
                                  nullValue = NA,
                                  envir = NULL) {
  assert_that(is.lcMethod(x),
              is.flag(eval),
              length(nullValue) == 1)

  if (isTRUE(eval)) {
    envir = lcMethod.env(x, parent.frame(), envir)
    evalClasses = c('NULL',
                    'logical',
                    'numeric',
                    'complex',
                    'integer',
                    'character',
                    'factor')
    method = evaluate.lcMethod(x, classes = evalClasses, envir = envir)
  } else {
    method = x
  }
  argList = as.list(method, eval = FALSE)

  dfList = lapply(argList, function(a) {
    if (is.null(a)) {
      nullValue
    } else if (is.atomic(a)) {
      if (length(a) > 1) {
        dput(a)
      } else {
        a
      }
    } else {
      deparse(a) %>% paste0(collapse = '')
    }
  })

  assert_that(all(vapply(dfList, length, FUN.VALUE = 0) == 1))
  as.data.frame(dfList, stringsAsFactors = FALSE)
}


#' @title Select the preferred environment
#' @description Returns envir if specified. Otherwise, returns environment(object) if specified. The defaultEnvir is returned when the former two are NULL.
#' @keywords internal
lcMethod.env = function(object, defaultEnvir, envir) {
  assert_that(is.lcMethod(object))
  assert_that(is.null(defaultEnvir) || is.environment(defaultEnvir))
  assert_that(is.null(envir) || is.environment(envir))

  if (!is.null(envir)) {
    envir
  } else if (!is.null(environment(object))) {
    environment(object)
  } else {
    defaultEnvir
  }
}


#' @export
#' @title Generate a list of lcMethod objects
#' @description Generates a list of `lcMethod` objects for a sequence of argument values.
#' @param method The `lcMethod` to use as the template, which will be updated for each of the other arguments.
#' @param ... Any other arguments to update the `lcMethod` definition with. Values must be `scalar`, `vector`, `list`, or encapsulated in a `.()` call.
#' Arguments wrapped in `.()` are passed as-is to the model call, ensuring a readable method.
#' Arguments comprising a single `symbol` (e.g. a variable name) are interpreted as a constant. To force evaluation, specify `arg=(var)` or `arg=force(var)`.
#' Arguments of type `vector` or `list` are split across a series of method fit calls.
#' Arguments of type `scalar` are constant across the method fits.
#' If a `list` is intended to be passed as a constant argument, then specifying `arg=.(listObject)` results in it being treated as such.
#' @param envir The `environment` in which to evaluate the method arguments.
#' @return A `list` of `lcMethod` objects.
#' @examples
#' kml = lcMethodKML()
#' methods = lcMethods(kml, nClusters=1:6)
#'
#' nclus = 1:6
#' methods = lcMethods(kml, nClusters=nclus)
#'
#' methods = lcMethods(kml, nClusters=3, center=.(meanNA, meanNA, median))
lcMethods = function(method, ..., envir = NULL) {
  assert_that(is.lcMethod(method))

  envir = lcMethod.env(method, parent.frame(), envir)

  mc = match.call()[-1]
  argNames = names(mc) %>% setdiff(c('', 'method', 'envir'))
  argCalls = mc[argNames]

  nameMask = vapply(argCalls, is.name, FUN.VALUE = FALSE)
  dotMask = vapply(argCalls, function(x)
    is.call(x) && x[[1]] == '.', FUN.VALUE = FALSE)
  evalMask = !nameMask & !dotMask
  evalArgs = lapply(argCalls[evalMask], eval, envir = parent.frame())

  dotLengths = vapply(argCalls[dotMask], length, FUN.VALUE = 0) - 1
  evalLengths = lengths(evalArgs)
  nModels = max(1, dotLengths, evalLengths)

  assert_that(
    all(c(dotLengths, evalLengths) %in% c(1L, nModels)),
    msg = sprintf(
      'arguments must be of length 1 or of equal length to all other arguments (%d)',
      nModels
    )
  )

  nameArgs = lapply(which(nameMask), function(i)
    as.list(argCalls[[i]]))
  dotArgs = lapply(which(dotMask), function(i)
    as.list(argCalls[[i]][-1]))

  firstOrN = function(x, i)
    x[[min(length(x), i)]]
  # using mapply results in dots[[1L]] errors
  methods = vector('list', nModels)
  for (i in seq_len(nModels)) {
    methods[[i]] = do.call(update,
                           c(
                             object = method,
                             lapply(nameArgs, firstOrN, i),
                             lapply(dotArgs, firstOrN, i),
                             lapply(evalArgs, firstOrN, i),
                             envir = envir
                           ))
  }

  assert_that(all(vapply(nameArgs, is.list, FUN.VALUE = FALSE)),
              all(vapply(dotArgs, is.list, FUN.VALUE = FALSE)),
              all(vapply(evalArgs, is.vector, FUN.VALUE = FALSE)), msg =
                'The processed argument lists are in an unexpected format. Please report this issue.')

  return(methods)
}


#. compose ####
#' @export
setGeneric('compose', function(method, ...)
  standardGeneric('compose'))
#' @export
#' @rdname lcMethod-interface
setMethod('compose', signature('lcMethod'), function(method, envir = NULL) {
  evaluate.lcMethod(method, try = FALSE, envir = envir)
})


# . fit ####
#' @export
setGeneric('fit', function(method, ...)
  standardGeneric('fit'))
#' @export
#' @rdname lcMethod-interface
#' @title lcMethod interface
#' @description Called by [latrend].
#' * compose
#' * validate
#' * prepareData
#' * preFit
#'
#' derpy
#' * fit
#' * postFit
setMethod('fit', signature('lcMethod'), function(method, data, envir, verbose) {
  stop(
    sprintf(
      'method cannot be estimated because the fit() function is not implemented for lcMethod of class %s.
   define the fit() method using:
      \tsetMethod("fit", signature("%s"), function(method, data, verbose) {
      \t\t<your code returning a lcModel-extended class here>
      \t})")'
    ),
    class(method)[1],
    class(method)[1]
  )
})


#' @export
#' @title Extract formula
#' @description Extracts the associated `formula` for the given distributional parameter.
#' @inheritParams as.list.lcMethod
#' @param what The distributional parameter to which this formula applies. By default, the formula specifies `"mu"`.
#' @return The `formula` for the given distributional parameter.
#' @examples
#' m = lcMethodKML(Value ~ Time)
#' formula(m) # Value ~ Time
#' @family lcMethod functions
formula.lcMethod = function(x, what = 'mu',
                            envir = NULL, ...) {
  assert_that(is.lcMethod(x))
  envir = lcMethod.env(x, parent.frame(), envir)
  assert_that(is.scalar(what), is.character(what))
  if (what == 'mu') {
    x$formula
  } else {
    x[[paste0('formula.', what)]]
  }
}



#' @export
#' @family lcMethod functions
getCall.lcMethod = function(x, ...) {
  assert_that(is.lcMethod(x))
  do.call(call, c(class(x)[1], eapply(x@arguments, enquote)))
}


#. getLabel ####
#' @export
setGeneric('getLabel', function(object, ...)
  standardGeneric('getLabel'))
#' @export
#' @rdname lcMethod-interface
setMethod('getLabel', signature('lcMethod'), function(object) {
  if (hasName(object, 'label')) {
    object$label
  } else {
    ''
  }
})


#. getName ####
#' @export
setGeneric('getName', function(object, ...)
  standardGeneric('getName'))

#' @export
#' @rdname lcMethod-interface
setMethod('getName', signature('lcMethod'), function(object)
  'custom')

#. getShortName ####
#' @export
setGeneric('getShortName', function(object, ...)
  standardGeneric('getShortName'))
#' @export
#' @rdname lcMethod-interface
setMethod('getShortName', signature('lcMethod'), getName)


#. idVariable ####
#' @export
setGeneric('idVariable', function(object, ...)
  standardGeneric('idVariable'))
#' @export
setMethod('idVariable', signature('lcMethod'), function(object)
  object$id)


#' @export
#' @title Check whether the argument of a lcMethod has a defined value.
#' @description Determines whether the associated argument value is defined. If the argument value is of type `language`, the argument is evaluated to see if it can be resolved within its `environment`.
#' @param name The name of the argument.
#' @param envir The `environment` to evaluate the arguments in. If `NULL`, the argument is not evaluated.
#' @keywords internal
isArgDefined = function(object, name, envir = environment(object)) {
  assert_that(is.lcMethod(object))
  assert_that(is.character(name), is.scalar(name))
  assert_that(is.environment(envir) || is.null(envir))

  if (!hasName(object, name)) {
    return(FALSE)
  }
  arg = object[[name[1], eval = FALSE]]


  if (is.language(arg)) {
    if (is.null(envir)) {
      return(FALSE)
    } else {
      arg = try(object[[name, envir = envir]], silent = TRUE)
      return(!is(arg, 'try-error'))
    }
  } else {
    return(TRUE)
  }
}


#' @export
is.lcMethod = function(object) {
  isS4(object) && is(object, 'lcMethod')
}


#. length ####
#' @export
setMethod('length', signature('lcMethod'), function(x) {
  length(x@arguments)
})


#. names ####
#' @title lcMethod argument names
#' @return A `character vector` of argument names.
#' @examples
#' m = lcMethodKML()
#' names(m)
#' @family lcMethod functions
setMethod('names', signature('lcMethod'), function(x) {
  argNames = names(x@arguments)
  if (is.null(argNames)) {
    character(0)
  } else {
    argNames
  }
})


# . preFit ####
#' @export
setGeneric('preFit', function(method, ...)
  standardGeneric('preFit'))
#' @rdname lcMethod-interface
setMethod('preFit', signature('lcMethod'), function(method, data, envir, verbose) {
  return(envir)
})


# . prepareData ####
#' @export
setGeneric('prepareData', function(method, ...)
  standardGeneric('prepareData'))
#' @rdname lcMethod-interface
setMethod('prepareData', signature('lcMethod'), function(method, data, verbose) {
  return(NULL)
})


# . postfit ####
#' @export
setGeneric('postFit', function(method, ...)
  standardGeneric('postFit'))
#' @rdname lcMethod-interface
setMethod('postFit', signature('lcMethod'), function(method, data, model, envir, verbose) {
  return(model)
})


#' @export
print.lcMethod = function(x,
                          ...,
                          eval = FALSE,
                          width = 40,
                          envir = NULL) {
  assert_that(is.lcMethod(x),
              is.flag(eval))
  envir = lcMethod.env(x, parent.frame(), envir)
  if (isTRUE(eval)) {
    x = evaluate.lcMethod(x, envir = envir)
  }

  arg2char = function(a) {
    if (is.null(a)) {
      'NULL'
    } else if (is.character(a)) {
      paste0('"', a, '"', collapse = ', ')
    } else if (is.atomic(a)) {
      paste0(as.character(a), collapse = ', ')
    } else {
      deparse(a) %>% paste0(collapse = '')
    }
  }

  argNames = names(x)
  chrValues = lapply(x@arguments, arg2char) %>% unlist()
  assert_that(all(vapply(chrValues, length, FUN.VALUE = 0) == 1))

  sourceMask = vapply(chrValues, nchar, FUN.VALUE = 0) > width &
    argNames %in% names(x@sourceCalls)
  chrSource = lapply(x@sourceCalls[argNames[sourceMask]], arg2char) %>% unlist()
  chrValues[sourceMask] = paste0('`', chrSource, '`')

  args = vapply(chrValues, strtrim, width = width, FUN.VALUE = '')

  if (length(args) > 0) {
    cat(sprintf(' %-16s%s\n', paste0(argNames, ':'), args), sep = '')
  } else {
    cat(' no arguments\n')
  }
}

#' @importFrom R.utils evaluate
#' @export
#' @title Substitute the call arguments for their evaluated values
#' @description Substitutes the call arguments if they can be evaluated without error.
#' @inheritParams as.list.lcMethod
#' @param classes Substitute only arguments with specific class types. By default, all types are substituted.
#' @param try Whether to try to evaluate arguments and ignore errors (the default), or to fail on any argument evaluation error.
#' @param exclude Arguments to exclude from evaluation.
#' @return A new `lcMethod` object with the substituted arguments.
#' @family lcMethod functions
evaluate.lcMethod = function(object,
                               classes = 'ANY',
                               try = TRUE,
                               exclude = character(),
                               envir = NULL) {
  assert_that(is.lcMethod(object),
              is.character(classes))

  envir = lcMethod.env(object, parent.frame(), envir)

  argNames = names(object)
  if (isTRUE(try)) {
    evalMask = vapply(
      argNames,
      isArgDefined,
      object = object,
      envir = envir,
      FUN.VALUE = FALSE
    ) & !(argNames %in% exclude)
  } else {
    evalMask = !(argNames %in% exclude)
  }

  evalValues = vector(mode = 'list', length = length(object))
  evalValues[evalMask] = lapply(argNames[evalMask], function(name)
    object[[name, eval = TRUE, envir = envir]])

  if ('ANY' %in% classes) {
    updateMask = evalMask
  } else {
    updateMask = evalMask & vapply(evalValues, class, FUN.VALUE = '') %in% classes
  }

  newmethod = object
  sourceMask = vapply(newmethod@arguments, is.language, FUN.VALUE = FALSE)
  sourceNames = argNames[updateMask & sourceMask]
  newmethod@sourceCalls[sourceNames] = mget(sourceNames, newmethod@arguments)

  updateNames = argNames[updateMask]
  updateValues = evalValues[updateMask]

  for (i in seq_along(updateNames)) {
    assign(updateNames[i], updateValues[[i]], pos = object@arguments)
  }
  # newmethod@arguments = replace(object@arguments, names(object)[updateMask], evalValues[updateMask])
  return(newmethod)
}



#' @export
#' @title Update a method specification
#' @details Updates or adds arguments to a `lcMethod` object. The inputs are evaluated in order to determine the presence of `formula` objects, which are updated accordingly.
#' @inheritParams as.list.lcMethod
#' @param .eval Whether to assign the evaluated argument values to the method. By default (`FALSE`), the argument expression is preserved.
#' @param .remove Names of arguments that should be removed.
#' @return The new `lcMethod` object with the additional or updated arguments.
#' @examples
#' m = lcMethodKML(Value ~ 1)
#' m2 = update(m, formula=~ . + Time)
#'
#' m3 = update(m2, start='randomAll')
#'
#' xvar = 2
#' m4 = update(m, x=xvar) # x: xvar
#'
#' m5 = update(m, x=xvar, .eval=TRUE) # x: 2
#'
#' @family lcMethod functions
update.lcMethod = function(object,
                           ...,
                           .eval = FALSE,
                           .remove = character(),
                           envir = NULL) {
  assert_that(is.lcMethod(object),
              is.flag(.eval),
              is.character(.remove))

  envir = lcMethod.env(object, parent.frame(), envir)

  argNames = names(object)
  if (isTRUE(.eval)) {
    ucall = list(...)
    uargValues = ucall
  } else {
    ucall = match.call()[c(-1, -2)]
    ucall$envir = NULL
    ucall$.eval = NULL
    ucall$.remove = NULL
    uargValues = lapply(ucall, eval, envir = envir)
  }
  uargNames = names(ucall)

  defMask = uargNames %in% argNames
  formulaMask = vapply(uargValues, is, 'formula', FUN.VALUE = FALSE)
  updateFormulaMask = formulaMask & defMask

  if (any(updateFormulaMask)) {
    oldFormulaArgs = lapply(uargNames[updateFormulaMask], function(name)
      object[[name]])
    ucall[updateFormulaMask] =
      mapply(update.formula, oldFormulaArgs, uargValues[updateFormulaMask], SIMPLIFY = FALSE) %>%
      lapply(match.call, definition = formula)
  }

  # copy environment
  object@arguments = list2env(as.list(object@arguments),
                              hash = FALSE,
                              parent = parent.env(object@arguments))

  for (arg in uargNames) {
    assign(arg, ucall[[arg]], pos = object@arguments)
  }
  #object@arguments = replace(object@arguments, uargNames, ucall[uargNames])
  object@sourceCalls[uargNames] = NULL

  if (length(.remove) > 0) {
    remove(list = .remove, pos = object@arguments)
    #object@arguments[.remove] = NULL
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
setMethod('responseVariable', signature('lcMethod'), function(object) {
  if (hasName(object, 'response')) {
    object$response
  } else if (hasName(object, 'formula')) {
    getResponse(object$formula)
  } else {
    stop(
      'cannot determine the response variable(s) for class ',
      class(object)[1],
      '\nConsider overriding "responseVariable(lcMethod)" to fix this for your lcMethod implementation'
    )
  }
})


#. show ####
setMethod('show', 'lcMethod', function(object) {
  cat(class(object)[1], ' as "', getName(object), '"\n', sep = '')
  print(object)
})


# . strip ####
#' @export
setGeneric('strip', function(object) standardGeneric('strip'))

#' @title Strip a lcMethod for serialization
#' @description Removes associated environments from any of the arguments. This typically is the case for formulas.
setMethod('strip', signature('lcMethod'), function(object) {
  newObject = object

  environment(newObject) = NULL
  newObject@arguments = eapply(object@arguments, 'environment<-', NULL) %>%
    list2env(hash = FALSE, parent = emptyenv())

  newObject@sourceCalls = lapply(object@sourceCalls, 'environment<-', NULL)

  return(newObject)
})


#. timeVariable ####
#' @export
setGeneric('timeVariable', function(object, ...) standardGeneric('timeVariable'))

#' @export
setMethod('timeVariable', signature('lcMethod'), function(object) object$time)


#. validate ####
#' @export
setGeneric('validate', function(method, data, ...) standardGeneric('validate'))

#' @export
#' @rdname lcMethod-interface
setMethod('validate', signature('lcMethod'), function(method, data, envir = NULL) {
  validate_that(
    hasName(data, idVariable(method)),
    hasName(data, timeVariable(method)),
    hasName(data, responseVariable(method)),
    is.character(getLabel(method))
  )
})


#' @export
match.call.all = function(definition = sys.function(sys.parent()),
                           call = sys.call(sys.parent()),
                           expand.dots = TRUE,
                           envir = parent.frame(2L)) {
  call = stackoverflow::match.call.defaults(definition = definition,
                                            call = call,
                                            expand.dots = expand.dots,
                                            envir = envir)
  # search for ..N arguments
  nameMask = vapply(call, is.name, FUN.VALUE = TRUE)
  dotMask = grepl('\\.\\.\\d+', as.character(call[nameMask]))

  if (any(dotMask)) {
    dotNames = names(call)[nameMask][dotMask]
    for (dotArg in dotNames) {
      call[[dotArg]] = do.call(substitute, list(as.name(dotArg)), envir = parent.frame())
    }
  }
  return (call)
}
