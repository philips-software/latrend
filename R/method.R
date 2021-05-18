#' @export
#' @name lcMethod-class
#' @rdname lcMethod-class
#' @title lcMethod class
#' @description Objects of type `lcMethod` enable the specification and estimation of different methods for longitudinal clustering.
#' The base class `lcMethod` provides the logic for storing, evaluating, and printing the method parameters.
#'
#' Subclasses of `lcMethod` differ only on the fitting procedure logic (see below).
#'
#' The intended way for specifying methods is through the method-specific constructor functions, e.g., through [lcMethodKML], [lcMethodLcmmGBTM], or [lcMethodDtwclust].
#'
#' @section Method arguments:
#' `lcMethod` objects represent the specification of a method with a set of configurable parameters (referred to as arguments).
#'
#' Arguments can be of any type. It is up to the `lcMethod` implementation of [validate()] to ensure that the required arguments are present and are of the expected type.
#'
#' Arguments can have almost any name. Exceptions include the names `"data"`, `"envir"`, and `"verbose"`. Furthermore, argument names may not start with a period (`"."`).
#'
#' Arguments cannot be directly modified, i.e., `lcMethod` objects are immutable. Modifying an argument involves creating a derivative object through the [update.lcMethod] method.
#'
#' @section Fitting procedure:
#' Each `lcMethod` subclass defines a type of methods in terms of a series of steps for estimating the method.
#' These steps, as part of the fitting procedure, are executed by [latrend()] in the following order:
#' \enumerate{
#'   \item [compose()]: Evaluate and finalize the method argument values.
#'   \item [validate()]: Check the validity of the method argument values.
#'   \item [prepareData()]: Process the training data for fitting.
#'   \item [preFit()]: Prepare environment for estimation, independent of training data.
#'   \item [fit()]: Estimate the specified method on the training data, outputting an object inheriting from `lcModel`.
#'   \item [postFit()]: Post-process the outputted `lcModel` object.
#' }
#'
#' The result of the fitting procedure is an [lcModel-class] object that inherits from the `lcModel` class.
#'
#' @details Because the `lcMethod` arguments may be unevaluated, argument retrieval functions such as `[[` accept an `envir` argument.
#' A default `environment` can be assigned or obtained from a `lcMethod` object using the `environment()` function.
#' @seealso [environment] [lcMethod-constructor]
#' @slot arguments A `list` representing the arguments of the `lcMethod` object. Arguments are not evaluated upon creation of the method object. Instead, arguments are stored similar to a `call` object. Do not modify or access.
#' @slot sourceCalls A list of calls for tracking the original call after substitution. Used for printing objects which require too many characters (e.g. ,function definitions, matrices).
#' @family lcMethod implementations
#' @family lcMethod functions
setClass('lcMethod', slots = c(arguments = 'environment', sourceCalls = 'list'))

#. initialize ####
setMethod('initialize', 'lcMethod', function(.Object, ...) {
  .Object = callNextMethod()
  validObject(.Object)
  .Object
})

#. validity ####
setValidity('lcMethod', function(object) {
  assert_that(all(vapply(lapply(names(object), nchar), '>', 0, FUN.VALUE = TRUE)),
    msg = 'lcMethod argument names cannot be empty')
  assert_that(!any(vapply(names(object), startsWith, '.', FUN.VALUE = TRUE)),
    msg = 'lcMethod argument names cannot start with "."')
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
#' @export
#' @name [[,lcMethod-method
#' @rdname indexy
#' @aliases $,lcMethod-method
#' @param name The argument name, as `character`.
#' @examples
#' m <- lcMethodKML(nClusters = 3)
#' m$nClusters # 3
setMethod('$', signature('lcMethod'), function(x, name) {
  x[[name]]
})


#. [[ ####
#' @export
#' @rdname indexy
#' @title Retrieve and evaluate a lcMethod argument by name
#' @param x The `lcMethod` object.
#' @param i Name or index of the argument to retrieve.
#' @param eval Whether to evaluate the call argument (enabled by default).
#' @param envir The `environment` in which to evaluate the argument. This argument is only applicable when `eval = TRUE`.
#' @return The argument `call` or evaluation result.
#' @examples
#' m = lcMethodKML(nClusters = 5)
#' m[["nClusters"]] # 5
#'
#' k = 2
#' m = lcMethodKML(nClusters = k)
#' m[["nClusters", eval=FALSE]] # k
#' @family lcMethod functions
setMethod('[[', signature('lcMethod'), function(x, i, eval = TRUE, envir = NULL) {
  envir = lcMethod.env(x, parent.frame(3), envir)
  if (is.character(i)) {
    assert_that(has_name(x, i), msg = sprintf('method does not have an argument named "%s"', i))
    arg = get(i, envir = x@arguments)
  } else {
    argName = names(x)[i]
    assert_that(!is.na(argName), msg = sprintf('index "%s" exceeded argument name options', i))
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
            sprintf('error in evaluating lcMethod argument "%s" with expression "%s":\n\t%s',
              i, deparse(e2$call), e2$message
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
#' @name lcMethod-constructor
#' @rdname lcMethod-constructor
#' @title Create a lcMethod object of the specified type and arguments
#' @description Note, see the [lcMethod-class][lcMethod-class] help page for documentation about the `lcMethod` class.
#'
#' Provides a mechanism for creating `lcMethod` objects for an arbitrary class.
#' However, it is advisable to use the subclass-specific constructors instead (e.g., [lcMethodLcmmGBTM()], [lcMethodKML()], or [lcMethodLMKM()]).
#' @param .class The type of `lcMethod` class
#' @param ... Any arguments to assign to the method object.
#' @param .defaults See `defaults` of [lcMethod.call].
#' @param .excludeArgs See `excludeArgs` of [lcMethod.call].
#' @section Implementation:
#' When creating your own `lcMethod` subclass, it is recommended to provide a constructor function for the subclass.
#' Sensible default method argument values are are specified through the function arguments.
#'
#' The [lcMethod.call()] function provides a convenient way to initialize the method according to the user-specified and default function arguments.
#'
#' \preformatted{
#' lcMethodExample <- function(
#'   response,
#'   time = getOption("latrend.time"),
#'   id = getOption("latrend.id"),
#'   nClusters = 2,
#'   # custom method argument defaults
#'   extraNum = 1,
#'   myOptAlgo = "EM",
#'   algoFunction = mean,
#'   ...
#' ) {
#'   lcMethod.call(
#'     "lcMethodExample",
#'     call = match.call.defaults()
#'   )
#' }
#' }
#'
#' Users can then specify `lcMethodExample` objects using the `lcMethodExample()` function.
#' For example, by calling `lcMethodExample(response = "Y", nClusters = 3)`.
#'
#' @seealso [lcMethod-class] [lcMethod.call]
lcMethod = function(.class, ..., .defaults = list(), .excludeArgs = c()) {
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
#' data(latrendData)
#' lcMethodKML2 <- function(response = "Y", id = "Id", time = "Time", nClusters = 2, ...) {
#'   lcMethod.call("lcMethodKML", call = stackoverflow::match.call.defaults(),
#'     defaults = c(kml::kml, kml::parALGO),
#'     excludeArgs = c("object", "nbClusters", "parAlgo", "toPlot", "saveFreq"))
#' }
#' method <- lcMethodKML2(nClusters = 3)
#' latrend(method, data = latrendData)
#' @seealso [lcMethod]
lcMethod.call = function(Class,
                          call,
                          defaults = list(),
                          excludeArgs = c()) {
  classRep = getClass(Class)
  assert_that('lcMethod' %in% names(classRep@contains), msg = 'specified class does not inherit from lcMethod')
  assert_that(
    is.call(call),
    is.function(defaults) ||
      is.list(defaults) &&
      all(vapply(defaults, is.function, FUN.VALUE = TRUE)),
    is.null(excludeArgs) || is.character(excludeArgs))

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
#' @param x The `lcMethod` object.
#' @param ... Additional arguments.
#' @param args A `character vector` of argument names to select. Only available arguments are returned.
#' Alternatively, a `function` or `list` of `function`s, whose formal arguments will be selected from the method.
#' @param eval Whether to evaluate the arguments.
#' @param expand Whether to return all method arguments when `"..."` is present among the requested argument names.
#' @param envir The `environment` in which to evaluate the arguments. If `NULL`, the environment associated with the object is used. If not available, the `parent.frame()` is used.
#' @return A `list` with the argument `call`s or evaluated results depending on the value for `eval`.
#' @examples
#' data(latrendData)
#' method <- lcMethodKML("Y", id = "Id", time = "Time")
#' as.list(method)
#'
#' as.list(method, args = c('id', 'time'))
#'
#' # select arguments used by kml()
#' as.list(method, args = kml::kml)
#'
#' # select arguments used by either kml() or parALGO()
#' as.list(method, args = c(kml::kml, kml::parALGO))
#' @family lcMethod functions
as.list.lcMethod = function(x, ..., args = names(x), eval = TRUE, expand = FALSE, envir = NULL) {
  assert_that(
    is.lcMethod(x),
    is.flag(eval),
    is.flag(expand)
  )

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
#' @param ... Additional arguments.
#' @param eval Whether to evaluate the arguments in order to replace expression if the resulting value is of a class specified in `evalClasses`.
#' @param nullValue Value to use to represent the `NULL` type. Must be of length 1.
#' @return A single-row `data.frame` where each columns represents an argument call or evaluation.
#' @family lcMethod functions
as.data.frame.lcMethod = function(x, ..., eval = FALSE, nullValue = NA, envir = NULL) {
  assert_that(
    is.lcMethod(x),
    is.flag(eval),
    length(nullValue) == 1
  )

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
        deparse(a) %>% as.character()
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

#' @noRd
#' @title Select the preferred environment
#' @description Returns envir if specified. Otherwise, returns environment(object) if specified. The defaultEnvir is returned when the former two are NULL.
#' @keywords internal
lcMethod.env = function(object, defaultEnvir, envir) {
  assert_that(
    is.lcMethod(object),
    is.null(defaultEnvir) || is.environment(defaultEnvir),
    is.null(envir) || is.environment(envir)
  )

  if (!is.null(envir)) {
    envir
  } else if (!is.null(environment(object))) {
    environment(object)
  } else {
    defaultEnvir
  }
}


#. compose ####
#' @export
#' @name compose
#' @aliases compose,lcMethod-method
#' @title Compose an lcMethod object
#' @description Evaluate and finalize the arguments of the given object inheriting from `lcMethod`.
#' The default implementation returns an updated object with all arguments having been evaluated.
#' @param method The `lcMethod` object.
#' @param envir The `environment` in which the `lcMethod` should be evaluated
#' @return The evaluated and finalized `lcMethod` object.
#' @inheritSection lcMethod-class Fitting procedure
#' @section Implementation:
#' In general, there is no need to extend this method for a specific method, as all arguments are automatically evaluated by the `compose,lcMethod` method.
#'
#' However, in case there is a need to extend processing or to prevent evaluation of specific arguments (e.g., for handling errors), the method can be overridden for the specific `lcMethod` subclass.
#' \preformatted{
#' setMethod("compose", "lcMethodExample", function(method, envir = NULL) {
#'   newMethod <- callNextMethod()
#'   # further processing
#'   return (newMethod)
#' })
#' }
#' @seealso [evaluate.lcMethod]
setMethod('compose', signature('lcMethod'), function(method, envir = NULL) {
  evaluate.lcMethod(method, try = FALSE, envir = envir)
})


# . fit ####
#' @export
#' @name fit
#' @aliases fit,lcMethod-method
#' @title Fit an lcMethod object to the processed data
#' @description Estimates the model as determined by the evaluated method specification, processed training data, and prepared environment.
#' @inheritParams preFit
#' @param envir The `environment` containing variables generated by [prepareData()] and [preFit()].
#' @return The fitted object inheriting from `lcModel`.
#' @section Implementation:
#' This method should be implemented for all `lcMethod` subclasses.
#'
#' \preformatted{
#' setMethod("fit", "lcMethodExample", function(method, data, envir, verbose) {
#'   # estimate the model or cluster parameters
#'   coefs <- FIT_CODE
#'
#'   # create the lcModel object
#'   new("lcModelExample",
#'     method = method,
#'     data = data,
#'     model = coefs,
#'     clusterNames = make.clusterNames(method$nClusters)
#'   )
#' })
#' }
#' @inheritSection lcMethod-class Fitting procedure
setMethod('fit', signature('lcMethod'), function(method, data, envir, verbose) {
  stop(
    sprintf('method cannot be estimated because the fit() function is not implemented for lcMethod of class %1$s.
   define the fit() method using:
      \tsetMethod("fit", signature("%1$s"), function(method, data, verbose) {
      \t\t<your code returning a lcModel-extended class here>
      \t})")', class(method)[1])
  )
})


#' @export
#' @title Extract formula
#' @description Extracts the associated `formula` for the given distributional parameter.
#' @inheritParams as.list.lcMethod
#' @param x The `lcMethod` object.
#' @param ... Additional arguments.
#' @param what The distributional parameter to which this formula applies. By default, the formula specifies `"mu"`.
#' @return The `formula` for the given distributional parameter.
#' @examples
#' m <- lcMethodMixtoolsGMM(formula = Y ~ Time + (1 | Id))
#' formula(m) # Y ~ Time + (1 | Id)
#' @family lcMethod functions
formula.lcMethod = function(x, what = 'mu', envir = NULL, ...) {
  assert_that(
    is.lcMethod(x),
    is.scalar(what),
    is.character(what)
  )

  envir = lcMethod.env(x, parent.frame(), envir)

  if (what == 'mu') {
    f = x$formula
  } else {
    f = x[[paste0('formula.', what)]]
  }
  environment(f) = envir
  f
}



#' @export
getCall.lcMethod = function(x, ...) {
  assert_that(is.lcMethod(x))
  do.call(call, c(class(x)[1], eapply(x@arguments, enquote)))
}


#. getLabel ####
#' @export
#' @name getLabel
#' @rdname getLabel
#' @aliases getLabel,lcMethod-method
#' @title Extract the method label.
#' @description Extracts the assigned label from the given `lcMethod` or `lcModel` object.
#' By default, the label is determined from the `"label"` argument of the `lcMethod` object.
#' The label of an `lcModel` object is set upon estimation by [latrend()] to the label of its associated `lcMethod` object.
#' @param object The `lcMethod` or `lcModel` object.
#' @param ... Additional arguments.
#' @return The extracted label, as `character`.
#' @seealso [getName] [getShortName]
#' @examples
#' getLabel(lcMethodKML()) # ""
#'
#' getLabel(lcMethodKML(label = "v2")) # "v2"
setMethod('getLabel', signature('lcMethod'), function(object, ...) {
  if (hasName(object, 'label')) {
    object$label
  } else {
    ''
  }
})


#. getName ####
#' @export
#' @name getName
#' @rdname getName
#' @aliases getName,lcMethod-method
#' @title Get the (short) name of the lcMethod or Model
#' @description Extract the full or shortened name of the given `lcMethod` or `lcModel` object.
#' The name of the fitted `lcModel` is determined by its associated `lcMethod` name and label, unless specified otherwise.
#' @param object The `lcMethod` or `lcModel` object.
#' @param ... Additional arguments.
#' @return A `character` name.
#' @section Implementation:
#' When implementing your own `lcMethod` subclass, override these methods to provide full and abbreviated names.
#' \preformatted{
#' setMethod("getName", "lcMethodExample", function(object) "example name")
#'
#' setMethod("getShortName", "lcMethodExample", function(object) "EX")
#' }
#'
#' Similar methods can be implemented for your `lcModel` subclass,
#' however in practice this is not needed as the names are determined by default from the `lcMethod` object that was used to fit the `lcModel` object.
#'
#' @seealso [getLabel]
#' @examples
#' getName(lcMethodKML()) # "longitudinal k-means"
setMethod('getName', signature('lcMethod'), function(object, ...) 'undefined')

#' @export
#' @name latrend-generics
#' @aliases getName,NULL-method
setMethod('getName', 'NULL', function(object, ...) 'null')

#. getShortName ####
#' @export
#' @name getShortName
#' @rdname getName
#' @aliases getShortName,lcMethod-method
#' @title Extract the short object name
#' @examples
#' getShortName(lcMethodKML()) # "KML"
setMethod('getShortName', signature('lcMethod'), getName)

#' @export
#' @name latrend-generics
#' @aliases getShortName,NULL-method
setMethod('getShortName', 'NULL', function(object, ...) 'nul')


#. idVariable ####
#' @export
#' @name idVariable
#' @rdname idVariable
#' @aliases idVariable,lcMethod-method
#' @title Extract the trajectory identifier variable
#' @description Extracts the trajectory identifier variable (i.e., column name) from the given `object`.
#' @param object The object to extract the variable from.
#' @param ... Not used.
#' @return The trajectory identifier name, as `character`.
#' @examples
#' method <- lcMethodKML(id = "Traj")
#' idVariable(method) # "Traj"
#'
setMethod('idVariable', signature('lcMethod'), function(object, ...) object$id)

#' @export
#' @title Check whether the argument of a lcMethod has a defined value.
#' @description Determines whether the associated argument value is defined. If the argument value is of type `language`, the argument is evaluated to see if it can be resolved within its `environment`.
#' @param object The `lcMethod` object.
#' @param name The name of the argument, as `character`.
#' @param envir The `environment` to evaluate the arguments in. If `NULL`, the argument is not evaluated.
#' @keywords internal
isArgDefined = function(object, name, envir = environment(object)) {
  assert_that(
    is.lcMethod(object),
    is.character(name),
    is.scalar(name),
    is.environment(envir) || is.null(envir)
  )

  if (!hasName(object, name)) {
    return(FALSE)
  }
  arg = object[[name[1], eval = FALSE]]


  if (is.language(arg)) {
    if (is.null(envir)) {
      return(FALSE)
    } else {
      arg = try(object[[name[1], envir = envir]], silent = TRUE)
      return(!is(arg, 'try-error'))
    }
  } else {
    return(TRUE)
  }
}


#' @export
#' @name latrend-is
#' @rdname is
#' @title Check if object is of Class
#' @param x The object to check the class of.
#' @keywords internal
is.lcMethod = function(x) {
  isS4(x) && inherits(x, 'lcMethod')
}


#. length ####
#' @export
#' @name names,lcMethod-method
#' @rdname names-lcMethod-method
#' @aliases length,lcMethod-method
#' @return The number of arguments, as `integer`.
setMethod('length', signature('lcMethod'), function(x) {
  length(x@arguments)
})


#. names ####
#' @export
#' @title lcMethod argument names
#' @description Extract the argument names or number of arguments from an `lcMethod` object.
#' @param x The `lcMethod` object.
#' @return A `character vector` of argument names.
#' @examples
#' m = lcMethodKML()
#' names(m)
#' length(m)
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
#' @name preFit
#' @aliases preFit,lcMethod-method
#' @title Prepare environment for fitting
#' @description Perform preparatory work that is needed for fitting the method, but should not be counted towards the method estimation time.
#' The work is added to the provided `environment`, allowing the [fit()] function to make use of the prepared work.
#' @inheritParams prepareData
#' @param envir The `environment` containing additional data variables returned by [prepareData()].
#' @section Implementation:
#' \preformatted{
#' setMethod("preFit", "lcMethodExample", function(method, data, envir, verbose) {
#'   # update envir with additional computed work
#'   envir$x <- INTENSIVE_OPERATION
#'   return (envir)
#' })
#' }
#' @inheritSection lcMethod-class Fitting procedure
#' @return The updated `environment` that will be passed to [fit()].
setMethod('preFit', signature('lcMethod'), function(method, data, envir, verbose) {
  envir
})


# . prepareData ####
#' @name prepareData
#' @aliases prepareData,lcMethod-method
#' @title Prepare the training data for fitting
#' @description Post-process the training data to meet the method requirements, such as transforming the data to a matrix, truncating the response variable, or creating additional data objects.
#'
#' The computed variables are stored in an `environment` which is passed to the [preFit()] function for further processing.
#'
#' By default, this method does not do anything.
#'
#' @inheritParams validate
#' @param verbose A [R.utils::Verbose] object indicating the level of verbosity.
#' @return An `environment` with the prepared data variable(s) that will be passed to [preFit()].
#' @section Implementation:
#' A common use case for this method is when the internal method fitting procedure expects the data in a different format.
#' In this example, the method converts the training data `data.frame` to a `matrix` of repeated and aligned trajectory measurements.
#' \preformatted{
#' setMethod("prepareData", "lcMethodExample", function(method, data, verbose) {
#'   envir = new.env()
#'   # transform the data to matrix
#'   envir$dataMat = dcastRepeatedMeasures(data,
#'     id = idColumn, time = timeColumn, response = valueColumn)
#'   return (envir)
#' })
#' }
#' @inheritSection lcMethod-class Fitting procedure
setMethod('prepareData', signature('lcMethod'), function(method, data, verbose) {
  new.env(parent = emptyenv())
})


# . postFit ####
#' @name postFit
#' @aliases postFit,lcMethod-method
#' @title Post-process the fitted lcModel object
#' @description Post-process the `lcModel` object returned by [fit()]. This can be used, for example, to (pre)compute additional metrics.
#' By default, this method does not do anything and returns the original `lcModel` object.
#'
#' This is the last step in the `lcMethod` fitting procedure. The `postFit` method may be called again on fitted `lcModel` objects, allowing post-processing to be updated for existing models.
#'
#' @inheritParams fit
#' @param model The `lcModel` object returned by [fit()].
#' @return The updated `lcModel` object.
#' @section Implementation:
#' The method is intended to be able to be called on previously fitted `lcModel` objects as well, allowing for potential bugfixes or additions to previously fitted models.
#' Therefore, when implementing this method, ensure that you do not discard information from the model which would prevent the method from being run a second time on the object.
#'
#' In this example, the `lcModelExample` class is assumed to be defined with a slot named `"centers"`:
#' \preformatted{
#' setMethod("postFit", "lcMethodExample", function(method, data, model, envir, verbose) {
#'   # compute and store the cluster centers
#'   model@centers <- INTENSIVE_COMPUTATION
#'   return (model)
#' })
#' }
#' @inheritSection lcMethod-class Fitting procedure
setMethod('postFit', signature('lcMethod'), function(method, data, model, envir, verbose) {
  model
})


#' @export
#' @title Print the arguments of an lcMethod object
#' @param x The `lcMethod` object.
#' @param eval Whether to print the evaluated argument values.
#' @param width Maximum number of characters per argument.
#' @param envir The environment in which to evaluate the arguments when `eval = TRUE`.
#' @param ... Not used.
print.lcMethod = function(x, ..., eval = FALSE, width = 40, envir = NULL) {
  assert_that(
    is.lcMethod(x),
    is.flag(eval)
  )

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
#' @param object The `lcMethod` object.
#' @param classes Substitute only arguments with specific class types. By default, all types are substituted.
#' @param try Whether to try to evaluate arguments and ignore errors (the default), or to fail on any argument evaluation error.
#' @param exclude Arguments to exclude from evaluation.
#' @return A new `lcMethod` object with the substituted arguments.
#' @seealso [compose]
#' @family lcMethod functions
evaluate.lcMethod = function(object,
                               classes = 'ANY',
                               try = TRUE,
                               exclude = character(),
                               envir = NULL) {
  assert_that(
    is.lcMethod(object),
    is.character(classes)
  )

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
#' @param object The `lcMethod` object.
#' @param ... The new or updated method argument values.
#' @param .eval Whether to assign the evaluated argument values to the method. By default (`FALSE`), the argument expression is preserved.
#' @param .remove Names of arguments that should be removed.
#' @return The new `lcMethod` object with the additional or updated arguments.
#' @examples
#' m <- lcMethodMixtoolsGMM(Value ~ 1)
#' m2 <- update(m, formula = ~ . + Time)
#'
#' m3 <- update(m2, nClusters = 3)
#'
#' k <- 2
#' m4 <- update(m, nClusters = k) # nClusters: k
#'
#' m5 <- update(m, nClusters = k, .eval = TRUE) # nClusters: 2
#'
#' @family lcMethod functions
update.lcMethod = function(object, ..., .eval = FALSE, .remove = character(), envir = NULL) {
  assert_that(
    is.lcMethod(object),
    is.flag(.eval),
    is.character(.remove)
  )

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
#' @name responseVariable
#' @rdname responseVariable
#' @aliases responseVariable,lcMethod-method
#' @title Extract the response variable
#' @description Extracts the response variable from the given `object`.
#' @param object The object to extract the response variable from.
#' @param ... Additional arguments.
#' @return The response variable name as a `character`.
#' @details If the `lcMethod` object specifies a `formula` argument, then the response is extracted from the response term of the formula.
#' @examples
#' method <- lcMethodKML("Value")
#' responseVariable(method) # "Value"
#'
#' method <- lcMethodLcmmGBTM(fixed = Value ~ Time, mixture = ~ Time)
#' responseVariable(method) # "Value"
#'
setMethod('responseVariable', signature('lcMethod'), function(object, ...) {
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
#' @name strip
#' @rdname strip
#' @aliases strip,lcMethod-method
setMethod('strip', signature('lcMethod'), function(object, ..., classes = 'formula') {
  newObject = object

  environment(newObject) = emptyenv()

  classMask = eapply(object@arguments, inherits, what = classes, all.names = TRUE) %>%
    unlist()

  if (any(classMask)) {
    stripArgNames = names(classMask)[classMask]
    newArgs = as.list(object@arguments, all.names = TRUE)
    stripArgs = mget(stripArgNames, envir = object@arguments) %>%
      lapply('environment<-', value = emptyenv())

    newArgs = replace(newArgs, which(classMask), stripArgs)
    newObject@arguments = list2env(newArgs, hash = FALSE, parent = emptyenv())
  }

  newObject@sourceCalls = lapply(object@sourceCalls, strip, ..., classes = classes)

  return(newObject)
})


#' @export
#' @name strip
#' @rdname strip
#' @aliases strip,ANY-method
setMethod('strip', signature('ANY'), function(object, ..., classes = 'formula') {
  if (is.list(object) || is(object, 'call')) { # is.call is TRUE for formulas
    replace(object, seq_along(object), lapply(object, strip))
  }
  else if (inherits(object, what = classes)) {
    environment(object) = emptyenv()
    object
  }
})


#. timeVariable ####
#' @export
#' @name timeVariable
#' @rdname timeVariable
#' @aliases timeVariable,lcMethod-method
#' @title Extract the time variable
#' @description Extracts the time variable (i.e., column name) from the given `object`.
#' @param object The object to extract the variable from.
#' @param ... Additional arguments.
#' @return The time variable name, as `character`.
#' @examples
#' method <- lcMethodKML(time = "Assessment")
#' timeVariable(method) # "Assessment"
#'
setMethod('timeVariable', signature('lcMethod'), function(object, ...) object$time)


#. validate ####
#' @export
#' @name validate
#' @aliases validate,lcMethod-method
#' @title Validate the argument values of a lcMethod object
#' @inheritParams compose
#' @param method An object inheriting from `lcMethod` with all its arguments having been evaluated and finalized.
#' @param data A `data.frame` representing the transformed training data.
#' @param ... Not used.
#' @return Either `TRUE` if all validation checks passed,
#' or a `character` containing a description of the failed validation checks.
#' @inheritSection lcMethod-class Fitting procedure
#' @section Implementation:
#' An example implementation checking for the existence of specific arguments and type:
#' \preformatted{
#' library(assertthat)
#' setMethod("validate", "lcMethodExample", function(method, data, envir = NULL, ...) {
#'   validate_that(
#'     hasName(method, "myArgument"),
#'     hasName(method, "anotherArgument"),
#'     is.numeric(method$myArgument)
#'   )
#' })
#' }
#' @seealso [assertthat::validate_that]
setMethod('validate', signature('lcMethod'), function(method, data, envir = NULL, ...) {
  validate_that(
    hasName(data, idVariable(method)),
    hasName(data, timeVariable(method)),
    hasName(data, responseVariable(method)),
    is.character(getLabel(method))
  )
})


#' @export
#' @title Argument matching with defaults and parent ellipsis expansion
#' @param definition A `function`. By default, the calling function is used.
#' @param call A `call`. By default, the parent call is used.
#' @param expand.dots Whether arguments specified in ellipsis should be included, or ellipses be kept as-is.
#' @param envir The `environment` in which the ellipsis and parent ellipsis are evaluated.
#' @seealso [stackoverflow::match.call.defaults]
#' @keywords internal
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
