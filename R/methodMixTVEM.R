#' @include method.R

#' @name interface-mixtvem
#' @rdname interface-mixtvem
#' @title mixtvem interface
#' @seealso [lcMethodMixTVEM]
#' @keywords internal
NULL

setClass('lcMethodMixTVEM', contains = 'lcMethod')

setValidity('lcMethodMixTVEM', function(object) {
  if (isArgDefined(object, 'formula')) {
    f = formula(object)
    assert_that(hasSingleResponse(f))

    f.t = getSpecialFormula(object$formula, special = 'time')
    f.x = dropSpecial(object$formula, special = 'time')

    if (hasIntercept(f)) {
      warning('only a time-varying intercept is supported. ~1 will be ignored')
    }

    tvars = getCovariates(f.t)
    xvars = getCovariates(f.x)
    vars = union(tvars, xvars)

    assert_that(hasIntercept(f.t) ||
                  length(tvars) > 0, msg = 'formula must contain time-varying intercept or one or more time-varying covariates')

    if (isArgDefined(object, 'time') && object$time %in% vars) {
      warning(
        'time occurs in the formula. This is not recommended for MixTVEM as time is already modeled through the coefficient functions.'
      )
    }

    assert_that(!hasRE(f), msg = 'random effects are not supported')
  }

  if (isArgDefined(object, 'formula.mb')) {
    fmb = formula(object, 'mb')
    assert_that(!hasResponse(fmb))
    assert_that(hasIntercept(fmb))
  }

  if (isArgDefined(object, 'maxIterations'))
    assert_that(
      is.scalar(object$maxIterations),
      is.numeric(object$maxIterations),
      object$maxIterations >= 0
    )
  if (isArgDefined(object, 'numInteriorKnots'))
    assert_that(is.count(object$numInteriorKnots))
  if (isArgDefined(object, 'deg'))
    assert_that(is.count(object$deg))
  if (isArgDefined(object, 'gridSize'))
    assert_that(is.count(object$gridSize))
  if (isArgDefined(object, 'maxVarianceRatio'))
    assert_that(is.scalar(object$maxVarianceRatio),
                is.numeric(object$maxVarianceRatio))
  if (isArgDefined(object, 'convergenceCriterion'))
    assert_that(
      is.scalar(object$convergenceCriterion),
      is.numeric(object$convergenceCriterion)
    )
  if (isArgDefined(object, 'nClusters'))
    assert_that(is.count(object$nClusters))
})

#' @export
#' @title Specify a MixTVEM
#' @note In order to use this method, you must download and source MixTVEM.R. See the reference below.
#' @param formula A `formula` excluding the time component. Time-invariant covariates are detected automatically as these are a special case in MixTVEM.
#' @param formula.mb A `formula` for cluster-membership prediction. Covariates must be time-invariant. Furthermore, the formula must contain an intercept.
#' @param time The name of the time variable.
#' @param id The name of the trajectory identifier variable.
#' @param nClusters The number of clusters. This replaces the `numClasses` argument of the `TVEMMixNormal` function call.
#' @param ... Arguments passed to the `TVEMMixNormal()` function.
#' The following optional arguments are ignored: doPlot, getSEs, numClasses.
#' @references \url{https://github.com/dziakj1/MixTVEM}
#' @examples
#' \donttest{
#' # this example only runs if you download and place MixTVEM.R in your wd
#' try({
#'   source("MixTVEM.R")
#'   method = lcMethodMixTVEM(
#'     Value ~ time(1) - 1,
#'     time = 'Assessment',
#'     id = "Id",
#'     nClusters = 3
#'   )
#' })
#' }
#' @references
#' \insertRef{dziak2015modeling}{latrend}
lcMethodMixTVEM = function(
  formula,
  formula.mb =  ~ 1,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  nClusters = 2,
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodMixTVEM'
  do.call(new, as.list(mc))
}

#' @rdname interface-mixtvem
setMethod('getArgumentDefaults', signature('lcMethodMixTVEM'), function(object) {
  c(
    formals(lcMethodMixTVEM),
    formals(TVEMMixNormal),
    callNextMethod()
  )
})

#' @rdname interface-mixtvem
setMethod('getArgumentExclusions', signature('lcMethodMixTVEM'), function(object) {
  union(
    callNextMethod(),
    c('doPlot', 'getSEs', 'numClasses')
  )
})

#' @rdname interface-mixtvem
#' @inheritParams getName
setMethod('getName', signature('lcMethodMixTVEM'), function(object) 'mixture of time-varying effect models')

#' @rdname interface-mixtvem
setMethod('getShortName', signature('lcMethodMixTVEM'), function(object) 'mixtvem')

#' @rdname interface-mixtvem
setMethod('preFit', signature('lcMethodMixTVEM'), function(method, data, envir, verbose, ...) {
  e = new.env()
  f.t = getSpecialFormula(method$formula, special = 'time')
  f.x = dropSpecial(method$formula, special = 'time')
  tvars = getCovariates(f.t)
  xvars = getCovariates(f.x)

  cat(verbose, 'Constructing model frame...', level = verboseLevels$finest)
  df_model = model.frame(f.t, data = data)
  e$dep = df_model[[1]]
  tmat = as.matrix(df_model[-1])

  if (hasIntercept(f.t)) {
    e$tcov = cbind(1, tmat)
  } else {
    e$tcov = tmat
  }

  if (length(xvars) > 0) {
    xmat = model.matrix(f.x, data = data)
    e$xcov = xmat
  }

  return(e)
})

#' @rdname interface-mixtvem
#' @inheritParams fit
setMethod('fit', signature('lcMethodMixTVEM'), function(method, data, envir, verbose, ...) {
  args = c(as.list(envir), as.list(method))
  args$id = data[[idVariable(method)]]
  args$time = data[[timeVariable(method)]]
  args$doPlot = FALSE
  args$getSEs = FALSE
  args$min.time = NA
  args$max.time = NA
  args$numClasses = method$nClusters

  model = do.call(TVEMMixNormal, args[intersect(names(args), formalArgs(TVEMMixNormal))])

  new(
    'lcModelMixTVEM',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
