#' @include method.R

#' @name interface-mixtools
#' @rdname interface-mixtools
#' @title mixtools interface
#' @seealso [lcMethodMixtoolsGMM] [lcMethodMixtoolsNPRM] \link[mixtools]{regmixEM.mixed} \link[mixtools]{npEM}
#' @keywords internal
NULL

setClass('lcMethodMixtoolsGMM', contains = 'lcMethod')

#' @export
#' @title Specify mixed mixture regression model using mixtools
#' @inheritParams lcMethodGCKM
#' @param ... Arguments passed to [mixtools::regmixEM.mixed].
#' The following arguments are ignored: data, y, x, w, k, addintercept.fixed, verb.
#' @examples
#' \donttest{
#' data(latrendData)
#'
#' if (require("mixtools")) {
#'   method <- lcMethodMixtoolsGMM(
#'     formula = Y ~ Time + (1 | Id),
#'     id = "Id", time = "Time",
#'     nClusters = 3,
#'     arb.R = FALSE
#'   )
#' }
#' }
#' @family lcMethod implementations
#' @references
#' \insertRef{benaglia2009mixtools}{latrend}
lcMethodMixtoolsGMM = function(
  formula,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  nClusters = 2,
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodMixtoolsGMM'
  do.call(new, as.list(mc))
}

#' @rdname interface-mixtools
setMethod('getArgumentDefaults', 'lcMethodMixtoolsGMM', function(object) {
  c(
    formals(lcMethodMixtoolsGMM),
    formals(mixtools::regmixEM.mixed),
    callNextMethod()
  )
})

#' @rdname interface-mixtools
setMethod('getArgumentExclusions', 'lcMethodMixtoolsGMM', function(object) {
  union(
    callNextMethod(),
    c('data', 'y', 'x', 'w', 'k', 'addintercept.fixed', 'verb')
  )
})

#' @rdname interface-mixtools
setMethod('getCitation', 'lcMethodMixtoolsGMM', function(object, ...) {
  citation('mixtools')
})

#' @rdname interface-mixtools
#' @inheritParams getName
setMethod('getName', 'lcMethodMixtoolsGMM', function(object) 'growth mixture modeling using mixtools')

#' @rdname interface-mixtools
setMethod('getShortName', 'lcMethodMixtoolsGMM', function(object) 'gmm')

#' @rdname interface-mixtools
setMethod('preFit', 'lcMethodMixtoolsGMM', function(method, data, envir, verbose, ...) {
  e = new.env()

  # Parse formula
  f = formula(method)
  valueColumn = responseVariable(method)
  id = idVariable(method)
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
setMethod('fit', 'lcMethodMixtoolsGMM', function(method, data, envir, verbose, ...) {
  args = as.list(method, args = mixtools::regmixEM.mixed)
  args$y = envir$y
  args$x = envir$x
  args$w = envir$w
  args$k = method$nClusters
  args$addintercept.fixed = FALSE
  args$addintercept.random = FALSE
  args$verb = canShow(verbose, 'fine')

  model = do.call(mixtools::regmixEM.mixed, args)
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
