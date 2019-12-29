# Method ####
setClass('clMethod', slots=c(call='call'))

setMethod('initialize', 'clMethod', function(.Object, ...) {
  .Object = callNextMethod()
  validObject(.Object)
  .Object
})

setMethod('show', 'clMethod',
          function(object) {
            cat('Cluslong method "', getName(object), '"\n', sep='')
            clMethodPrintArgs(object)
          }
)


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
formula.clMethod = function(object, what='mu') {
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
setMethod('[[', signature('clMethod'), function(x, i, eval=TRUE, envir=parent.frame(3)) {
  if (is.character(i)) {
    assert_that(has_name(x, i), msg=sprintf('method does not have an argument named "%s"', i))
    arg = getCall(x)[[i]]
  } else {
    argName = names(x)[i]
    assert_that(!is.na(argName), msg=sprintf('index "%s" exceeded argument name options', i))
    arg = getCall(x)[[names(x)[i]]]
  }

  if(eval) {
    eval(arg, envir=envir, enclos=parent.env(getNamespace(.packageName)))
  } else {
    arg
  }
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
update.clMethod = function(object, ...) {
  ucall = match.call() %>% tail(-2)
  argNames = names(object)
  uargNames = names(ucall)
  assert_that(all(uargNames %in% argNames),
              msg=sprintf('attempted to update unsupported arguments %s', paste0('"', setdiff(uargNames, argNames), '"', collapse=', ')))
  uargValues = lapply(ucall, eval, envir=parent.frame())
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
as.list.clMethod = function(object, eval=TRUE, envir=parent.frame()) {
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
substitute.clMethod = function(object, envir=parent.frame()) {
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


#' # Control ####
#' setClass('clControl',
#'          representation(maxIter='numeric',
#'                         tolerance='numeric',
#'                         verbose='numeric'))
#'
#' #' @export
#' #' @title Create a cluslong method control object
#' clControl = function(maxIter=Inf, tolerance=1e-6, verbose=0) {
#'   cfg = mget(names(formals()), sys.frame(sys.nframe()))
#'   do.call(new, c('clControl', cfg))
#' }
#'
#' setMethod('show', 'clControl',
#'           function(object) {
#'             cat('Control object:\n')
#'             slots = slotNames(object)
#'             values = sapply(slots, slot, object=object)
#'             sprintf(' %-12s%10f', paste0(slots, ':'), values) %>% paste0(collapse='\n') %>% cat
#'           }
#' )