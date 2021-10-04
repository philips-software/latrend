#' @include method.R

setOldClass('lcMethods')

#' @export
#' @title Convert a list of lcMethod objects to a data.frame
#' @description Converts a list of `lcMethod` objects to a `data.frame`.
#' @inheritParams as.data.frame.lcMethod
#' @param x the `lcMethods` or `list` to be coerced to a `data.frame`.
#' @param ... Additional arguments.
#' @return A `data.frame` with each row containing the argument values of a method object.
#' @family lcMethod functions
as.data.frame.lcMethods = function(x, ..., eval = TRUE, nullValue = NA, envir = parent.frame()) {
  df = lapply(x, as.data.frame, ..., eval = eval, nullValue = nullValue, envir = envir) %>%
    rbindlist(fill = TRUE) %>%
    as.data.frame()

  cbind(.class = vapply(x, class, FUN.VALUE = ''), df)
}

#' @export
#' @title Convert a list of lcMethod objects to a lcMethods list
#' @param x A `list` of `lcMethod` objects.
#' @return A `lcMethods` object.
#' @family lcMethod functions
as.lcMethods = function(x) {
  if (!is.list(x)) {
    x = list(x)
  }

  class(x) = c('lcMethods', 'list')
  x
}

#' @export
#' @title Generate a list of lcMethod objects
#' @description Generates a list of `lcMethod` objects for all combinations of the provided argument values.
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
#' data(latrendData)
#' baseMethod <- lcMethodKML("Y", id = "Id", time = "Time")
#' methods <- lcMethods(baseMethod, nClusters = 1:6)
#'
#' nclus <- 1:6
#' methods <- lcMethods(baseMethod, nClusters = nclus)
#'
#' methods <- lcMethods(baseMethod, nClusters = 3, center = .(mean, mean, median))
#' length(methods) # 3
#'
#' methods <- lcMethods(baseMethod, nClusters = 1:3, center = .(mean, mean, median))
#' length(methods) # 9
lcMethods = function(method, ..., envir = NULL) {
  assert_that(is.lcMethod(method))

  envir = .selectEnvironment(method, parent.frame(), envir)

  mc = match.call()[-1]
  assert_that(not('' %in% names(mc)), msg = 'method arguments must be named')
  argNames = names(mc) %>% setdiff(c('method', 'envir'))
  argCalls = as.list(mc)[argNames]

  nameMask = vapply(argCalls, is.name, FUN.VALUE = FALSE)
  dotMask = vapply(argCalls, function(x) is.call(x) && x[[1]] == '.', FUN.VALUE = FALSE)
  evalMask = !nameMask & !dotMask

  nameArgs = lapply(
    which(nameMask),
    function(i) {
      tryCatch({
          argVal = eval(argCalls[[i]], envir = envir)
          as.list(argVal)
        },
        error = function(...) argCalls[i]
      )
    }
  )
  dotArgs = lapply(which(dotMask), function(i) as.list(argCalls[[i]][-1]))

  evalArgs = lapply(
    which(evalMask),
    function(i) {
      tryCatch({
          eval(argCalls[[i]], envir = envir)
        },
        error = function(e, ...) {
          stop(sprintf(
            'Error occurred while evaluating lcMethods argument "%s":\n  "%s"\n',
            argNames[i],
            e$message
          ))
        }
      )
    }
  )

  assert_that(
    all(vapply(nameArgs, is.list, FUN.VALUE = FALSE)),
    all(vapply(dotArgs, is.list, FUN.VALUE = FALSE)),
    all(vapply(evalArgs, is.vector, FUN.VALUE = FALSE)),
    msg = 'The processed argument lists are in an unexpected format. Please report this issue.'
  )

  # combine the different arguments into a name-sorted list
  allArgs = c(nameArgs, dotArgs, evalArgs)
  allArgs = allArgs[sort(names(allArgs))]
  argNs = lengths(allArgs)

  # generate combinations
  combIdx = lapply(argNs, seq_len) %>%
    expand.grid() %>%
    as.matrix()

  # generate method list
  if (nrow(combIdx) == 0) {
    methodList = list(method)
  }
  else {
    methodList = apply(combIdx, 1, function(idx) {
      methodArgs = mapply('[[', allArgs, idx, SIMPLIFY = FALSE)
      do.call(update, c(object = method, methodArgs, envir = envir))
    })
  }

  as.lcMethods(methodList)
}
