#' @include method.R

#' @name interface-mixtools
#' @rdname interface-mixtools
#' @title mixtools interface
#' @seealso [lcMethodMixtoolsGMM] [lcMethodMixtoolsNPRM] \link[mixtools]{regmixEM.mixed} \link[mixtools]{npEM}
NULL

setClass('lcMethodMixtoolsGMM', contains = 'lcMethod')

#' @export
#' @title Specify mixed mixture regression model using mixtools
#' @inheritParams lcMethodGCKM
#' @param ... Arguments passed to [mixtools::regmixEM.mixed].
#' The following arguments are ignored: data, y, x, w, k, addintercept.fixed, verb.
#' @examples
#' library(mixtools)
#' data(testLongData)
#' method <- lcMethodMixtoolsGMM(
#'    formula = Value ~ Time + (Time | Id),
#'    time = "Time",
#'    id = "Id",
#'    nClusters = 3)
#' model <- latrend(method, testLongData)
#' @family lcMethod implementations
lcMethodMixtoolsGMM = function(formula,
                               time = getOption('latrend.time'),
                               id = getOption('latrend.id'),
                               nClusters = 2,
                               ...) {
  lcMethod.call(
    'lcMethodMixtoolsGMM',
    call = match.call.defaults(),
    defaults = mixtools::regmixEM.mixed,
    excludeArgs = c('data', 'y', 'x', 'w', 'k', 'addintercept.fixed', 'verb')
  )
}

#' @rdname interface-mixtools
#' @inheritParams getName
setMethod('getName', signature('lcMethodMixtoolsGMM'), function(object) 'growth mixture modeling using mixtools')

#' @rdname interface-mixtools
setMethod('getShortName', signature('lcMethodMixtoolsGMM'), function(object) 'gmm')

#' @rdname interface-mixtools
setMethod('preFit', signature('lcMethodMixtoolsGMM'), function(method, data, envir, verbose, ...) {
  e = new.env()

  # Parse formula
  f = formula(method)
  valueColumn = responseVariable(method)
  id = idVariable(method)
  assert_that(!hasCLUSTER(f), msg = 'CLUSTER-specific fixed effects are not supported for this method')
  e$fixed = dropRE(f)
  reTerms = getREterms(f)
  if (length(reTerms) > 0) {
    e$random = reTerms[[1]] %>% REtermAsFormula
  } else {
    stop('no random effects specified')
  }

  # Response
  e$y = split(data[[valueColumn]], data[[id]])

  # Fixed effects
  W = model.matrix(e$fixed, data = data)
  e$w = as.data.frame(W) %>%
    split(data[[id]]) %>%
    lapply(as.matrix)

  # Random effects
  X = model.matrix(e$random, data = data)
  e$x = as.data.frame(X) %>%
    split(data[[id]]) %>% #split() outputs a vector for matrix input..
    lapply(as.matrix)

  return(e)
})

#' @rdname interface-mixtools
#' @inheritParams fit
setMethod('fit', signature('lcMethodMixtoolsGMM'), function(method, data, envir, verbose, ...) {
  args = as.list(method, args = mixtools::regmixEM.mixed)
  args$y = envir$y
  args$x = envir$x
  args$w = envir$w
  args$k = method$nClusters
  args$addintercept.fixed = FALSE
  args$addintercept.random = FALSE
  args$verb = canShow(verbose, 'fine')

  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  suppressFun({
    model = do.call(mixtools::regmixEM.mixed, args)
  })
  model$fixed = envir$fixed
  model$random = envir$random

  new(
    'lcModelMixtoolsGMM',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
