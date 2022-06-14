#' @include method.R

#' @name interface-mixAK
#' @rdname interface-mixAK
#' @title mixAK interface
#' @seealso [lcMethodMixAK_GLMM] [mixAK::GLMM_MCMC]
#' @keywords internal
NULL

setClass('lcMethodMixAK_GLMM', contains = 'lcMethod')

#' @export
#' @title Specify a GLMM iwht a normal mixture in the random effects
#' @param fixed A `formula` specifying the fixed effects of the model, including the response. Creates the `y` and `x` arguments for the call to [mixAK::GLMM_MCMC].
#' @param random A `formula` specifying the random effects of the model, including the random intercept. Creates the `z` and `random.intercept` arguments for the call to [mixAK::GLMM_MCMC].
#' @param time The name of the time variable.
#' @param id The name of the trajectory identifier variable. This is used to generate the `id` vector argument for the call to [mixAK::GLMM_MCMC].
#' @param nClusters The number of clusters.
#' @param ... Arguments passed to [mixAK::GLMM_MCMC].
#' The following external arguments are ignored: y, x, z, random.intercept, silent.
#' @family lcMethod implementations
#' @note This method currently does not appear to work under R 4.2 due to an error triggered by the mixAK package during fitting.
#' @examples
#' data(latrendData)
#' # this example only runs when the mixAK package is installed
#' try({
#'  method <- lcMethodMixAK_GLMM(fixed = Y ~ 1, random = ~ Time,
#'   id = "Id", time = "Time", nClusters = 3)
#'  model <- latrend(method, latrendData)
#'  summary(model)
#' })
#' @references
#' \insertRef{komarek2009new}{latrend}
lcMethodMixAK_GLMM = function(
  fixed,
  random,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  nClusters = 2,
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodMixAK_GLMM'
  do.call(new, as.list(mc))
}

#' @rdname interface-mixAK
setMethod('getArgumentDefaults', signature('lcMethodMixAK_GLMM'), function(object) {
  c(
    formals(lcMethodMixAK_GLMM),
    formals(mixAK::GLMM_MCMC),
    callNextMethod()
  )
})

#' @rdname interface-mixAK
setMethod('getArgumentExclusions', signature('lcMethodMixAK_GLMM'), function(object) {
  union(
    callNextMethod(),
    c('y', 'x', 'z', 'random.intercept', 'silent')
  )
})

#' @rdname interface-mixAK
#' @inheritParams getName
setMethod('getName', signature('lcMethodMixAK_GLMM'), function(object) 'generalized linear mixed model with normal random effects mixture')

#' @rdname interface-mixAK
setMethod('getShortName', signature('lcMethodMixAK_GLMM'), function(object) 'GLMMmix')

#' @rdname interface-mixAK
setMethod('responseVariable', signature('lcMethodMixAK_GLMM'), function(object) {
  getResponse(object$fixed)
})

#' @rdname interface-mixAK
setMethod('preFit', signature('lcMethodMixAK_GLMM'), function(method, data, envir, verbose, ...) {
  e = new.env()

  # create fixed effects matrix
  e$x = dropResponse(method$fixed) %>%
    dropIntercept() %>%
    model.matrix(data = data)

  if (ncol(e$x) == 0) {
    e$x = 'empty'
  }

  # create random effects matrix
  e$z = dropIntercept(method$random) %>%
    model.matrix(data = data)

  if (any(colnames(e$z) %in% colnames(e$x))) {
    warning('random effects terms should not be included in the fixed effects formula, as these are implicitely included')
  }

  e$random.intercept = hasIntercept(method$random)
  return(e)
})

#' @rdname interface-mixAK
#' @inheritParams fit
setMethod('fit', signature('lcMethodMixAK_GLMM'), function(method, data, envir, verbose, ...) {
  args = as.list(method, args = mixAK::GLMM_MCMC)
  args$y = data[[responseVariable(method)]]
  args$id = data[[idVariable(method)]]
  args$x = envir$x
  args$z = envir$z
  args$random.intercept = envir$random.intercept
  args$silent = as.logical(verbose)

  if(hasName(args, 'prior.b')) {
    args$prior.b = modifyList(args$prior.b, list(Kmax = method$nClusters))
  } else {
    args$prior.b = list(Kmax = method$nClusters)
  }

  model = do.call(mixAK::GLMM_MCMC, args)

  if (is(model, 'GLMM_MCMC')) {
    Class = 'lcModelMixAK_GLMM'
  } else {
    Class = 'lcModelMixAK_GLMMlist'
  }

  new(
    Class,
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
