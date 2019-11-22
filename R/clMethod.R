# Method ####
setClass('clMethod', slots=c(call='call'))

#' @export
setGeneric('getName', function(object) standardGeneric('getName'))

#' @export
setGeneric('getName0', function(object) standardGeneric('getName0'))

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
#' @examples
#' m = clMethodKML()
#' m$formula
setMethod('$', signature('clMethod'), function(x, name) {
  x[[name]]
})

#' @title Retrieve and evaluate a clMethod argument by name
#' @examples
#' m = clMethodKML()
#' m[['formula']]
setMethod('[[', signature('clMethod'), function(x, i) {
  if (is.character(i)) {
    assert_that(has_name(x, i), msg=sprintf('method does not have an argument named "%s"', i))
    arg = getCall(x)[[i]]
  } else {
    argName = names(x)[i]
    assert_that(!is.na(argName), msg=sprintf('index "%s" exceeded argument name options', i))
    arg = getCall(x)[[names(x)[i]]]
  }

  eval(arg, envir=list(globalenv()), enclos=parent.env(getNamespace(.packageName)))
})

setMethod('show', 'clMethod',
          function(object) {
            cat('Cluslong method "', getName(object), '"\n', sep='')
            clMethodPrintArgs(object)
          }
)

setGeneric('fit', function(object, ...) standardGeneric('fit'))

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
  uargValues = lapply(ucall, eval)
  formulaMask = sapply(uargValues, is, 'formula')

  if(any(formulaMask)) {
    oldFormulaArgs = lapply(uargNames[formulaMask], '$', x=object)
    ucall[formulaMask] = mapply(update.formula, oldFormulaArgs, uargValues[formulaMask], SIMPLIFY=FALSE) %>%
      lapply(match.call, definition=formula)
  }

  object@call = replace(getCall(object)[-1], uargNames, ucall[uargNames])
  validObject(object)
  return(object)
}

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