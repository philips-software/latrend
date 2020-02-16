#' @include clMethod.R
setClass('clMethodMixTVEM', contains='clMethod')

#' @export
#' @title Specify MixTVEM method
#' @param formula Formula excluding the time component. Covariates may be included. Time-invariant covariates are detected automatically as these are a special case in MixTVEM.
#' @param formula.mb Formula for cluster-membership prediction. Included covariates must be time-invariant. Furthermore, the formula must contain an intercept.
#' @param time Time variable.
#' @param id Strata variable.
#' @param nClusters Number of clusters.
#' @param numInteriorKnots Number of interior knots for the spline.
#' @param deg Degree of the local polynomials between intervals (knots).
#' @param numStarts Number of random starts.
#' @param maxIterations Maximum number of iterations for the EM algorithm.
#' @param maxVarianceRatio Maximum ratio that between-cluster variances may deviate from each other.
#' @examples
#' method = clMethodMixTVEM(Value ~ time(1) - 1,
#'                      time='Assessment',
#'                      id='Id', nClusters=3)
clMethodMixTVEM = function(formula=Value ~ time(1) - 1,
                           formula.mb=~1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       deg=3,
                       numInteriorKnots=6,
                       numStarts=1,
                       gridSize=1000,
                       maxIterations=1000,
                       maxVarianceRatio=10,
                       convergenceCriterion=1e-6) {
  object = new('clMethodMixTVEM', call=match.call.defaults())

  if(getOption('cluslong.checkArgs')) {
    checkArgs(object, envir=parent.frame())
  }
  return(object)
}

setMethod('checkArgs', signature('clMethodMixTVEM'), function(object, envir) {
  environment(object) = envir
  assert_that(all(formalArgs(clMethodMixTVEM) %in% names(getCall(object))), msg='clMethod object is missing required arguments')

  if(isArgDefined(object, 'formula')) {
    f = formula(object)
    assert_that(hasSingleResponse(f))

    f.t = getSpecialFormula(object$formula, special='time')
    f.x = dropSpecial(object$formula, special='time')

    if(hasIntercept(f)) {
      warning('only a time-varying intercept is supported. ~1 will be ignored')
    }

    tvars = getCovariates(f.t)
    xvars = getCovariates(f.x)
    vars = union(tvars, xvars)

    assert_that(hasIntercept(f.t) || length(tvars) > 0, msg='formula must contain time-varying intercept or one or more time-varying covariates')

    if(isArgDefined(object, 'time') && object$time %in% vars) {
      warning('time occurs in the formula. This is not recommended for MixTVEM as time is already modeled through the coefficient functions.')
    }

    assert_that(!hasRE(f), msg='random effects are not supported')
  }

  if(isArgDefined(object, 'formula.mb')) {
    fmb = formula(object, 'mb')
    assert_that(!hasResponse(fmb))
    assert_that(hasIntercept(fmb))
  }

  if(isArgDefined(object, 'maxIterations')) assert_that(is.scalar(object$maxIterations), is.numeric(object$maxIterations), object$maxIterations >= 0)
  if(isArgDefined(object, 'numInteriorKnots')) assert_that(is.count(object$numInteriorKnots))
  if(isArgDefined(object, 'deg')) assert_that(is.count(object$deg))
  if(isArgDefined(object, 'gridSize')) assert_that(is.count(object$gridSize))
  if(isArgDefined(object, 'maxVarianceRatio')) assert_that(is.scalar(object$maxVarianceRatio), is.numeric(object$maxVarianceRatio))
  if(isArgDefined(object, 'convergenceCriterion')) assert_that(is.scalar(object$convergenceCriterion), is.numeric(object$convergenceCriterion))
  if(isArgDefined(object, 'nClusters')) assert_that(is.count(object$nClusters))
})


setMethod('getName', signature('clMethodMixTVEM'), function(object) 'mixture of time-varying effect models')

setMethod('getName0', signature('clMethodMixTVEM'), function(object) 'mixtvem')


setMethod('prepare', signature('clMethodMixTVEM'), function(method, data, verbose, ...) {
  e = new.env()
  f.t = getSpecialFormula(method$formula, special='time')
  f.x = dropSpecial(method$formula, special='time')
  tvars = getCovariates(f.t)
  xvars = getCovariates(f.x)

  cat(verbose, 'Constructing model frame...', level=verboseLevels$finest)
  df_model = model.frame(f.t, data=data)
  e$dep = df_model[[1]]
  tmat = as.matrix(df_model[-1])

  if(hasIntercept(f.t)) {
    e$tcov = cbind(1, tmat)
  } else {
    e$tcov = tmat
  }

  if(length(xvars) > 0) {
    xmat = model.matrix(f.x, data=data)
    e$xcov = xmat
  }

  return(e)
})

setMethod('fit', signature('clMethodMixTVEM'), function(method, data, envir, verbose, ...) {
  e = new.env()

  args = c(as.list(envir), as.list(method))
  args$id = data[[method$id]]
  args$time = data[[method$time]]
  args$doPlot = FALSE
  args$getSEs = FALSE
  args$min.time = NA
  args$max.time = NA
  args$numClasses = method$nClusters

  suppressFun = ifelse(as.logical(verbose), force, capture.output)

  startTime = Sys.time()
  suppressFun({
    e$model = do.call(TVEMMixNormal, args[intersect(names(args), formalArgs(TVEMMixNormal))])
  })
  e$runTime = as.numeric(Sys.time() - startTime)

  return(e)
})

setMethod('finalize', signature('clMethodMixTVEM'), function(method, data, envir, verbose, ...) {
  model = new('clModelMixTVEM',
              method=method,
              data=data,
              model=envir$model,
              clusterNames=make.clusterNames(method$nClusters))
  return(model)
})