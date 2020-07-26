#' @include method.R
setClass('lcMethodMixAK_GLMM', contains = 'lcMethod')

#' @export
#' @inheritDotParams mixAK::GLMM_MCMC
#' @title Specify a GLMM iwht a normal mixture in the random effects
#' @param fixed Formula specifying the fixed effects of the model, including the response. Creates the \code{y} and \code{x} arguments for \link[mixAK]{GLMM_MCMC}.
#' @param random Formula specifying the random effects of the model, including the random intercept. Creates the \code{z} and \code{random.intercept} arguments for \link[mixAK]{GLMM_MCMC}.
#' @param time Time variable.
#' @param id Trajectory indicator column. Used to generate the \code{id} vector argument for the call.
#' @param nClusters Number of clusters.
#' @family lcMethod implementations
#' @examples
#' data(testLongData)
#' m = lcMethodMixAK_GLMM(Value ~ 1, random = ~ Time, nClusters = 2)
#' model = latrend(m, data = testLongData)
lcMethodMixAK_GLMM = function(fixed,
                              random,
                            time = getOption('latrend.time'),
                            id = getOption('latrend.id'),
                            nClusters = 2,
                            ...) {
  lcMethod.call(
    'lcMethodMixAK_GLMM',
    call = match.call.all(),
    defaults = mixAK::GLMM_MCMC,
    excludeArgs = c('y', 'x', 'z', 'random.intercept', 'silent')
  )
}

setMethod('getName', signature('lcMethodMixAK_GLMM'), function(object) 'generalized linear mixed model with normal random effects mixture')

setMethod('getShortName', signature('lcMethodMixAK_GLMM'), function(object) 'GLMMmix')

setMethod('responseVariable', signature('lcMethodMixAK_GLMM'), function(object) {
  getResponse(object$fixed)
})


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
