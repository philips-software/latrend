#' @include clMethod.R
setClass('clMethodMixtoolsGMM', contains='clMethod')

#' @export
#' @importFrom mixtools regmixEM.mixed
#' @title Specify mixed mixture regression model using mixtools
#' @inheritDotParams mixtools::regmixEM.mixed
#' @examples
#' method = clMethodMixtoolsGMM(Value ~ Time + (Time | Id),
#'                      time='Time',
#'                      id='Id', nClusters=3)
#' @family clMethod implementations
clMethodMixtoolsGMM = function(formula=Value ~ 1 + (Time + I(Time^2) | Id),
                               time=getOption('cluslong.time'),
                               id=getOption('cluslong.id'),
                               nClusters=2,
                               ...
) {
  clMethod('clMethodMixtoolsGMM', call=match.call.defaults(),
           defaults=mixtools::regmixEM.mixed,
           excludeArgs=c('data', 'y', 'x', 'w', 'k', 'addintercept.fixed', 'verb'))
}

setMethod('getName', signature('clMethodMixtoolsGMM'), function(object) 'growth mixture modeling using mixtools')

setMethod('getShortName', signature('clMethodMixtoolsGMM'), function(object) 'gmm')


setMethod('prepare', signature('clMethodMixtoolsGMM'), function(method, data, verbose, ...) {
  e = new.env()

  # Parse formula
  f = formula(method)
  valueColumn = getResponse(f)
  assert_that(!hasCLUSTER(f), msg='CLUSTER-specific fixed effects are not supported for this method')
  e$fixed = dropRE(f)
  reTerms = getREterms(f)
  if (length(reTerms) > 0) {
    e$random = reTerms[[1]] %>% REtermAsFormula
  } else {
    stop('no random effects specified')
  }

  # Response
  e$y = split(data[[valueColumn]], data[[method$id]])

  # Fixed effects
  W = model.matrix(e$fixed, data=data)
  e$w = as.data.frame(W) %>%
    split(data[[method$id]]) %>%
    lapply(as.matrix)

  # Random effects
  X = model.matrix(e$random, data=data)
  e$x = as.data.frame(X) %>%
    split(data[[method$id]]) %>% #split() outputs a vector for matrix input..
    lapply(as.matrix)

  return(e)
})

#' @importFrom mixtools regmixEM.mixed
setMethod('fit', signature('clMethodMixtoolsGMM'), function(method, data, envir, verbose, ...) {
  args = as.list(method, args=mixtools::regmixEM.mixed)
  args$y = envir$y
  args$x = envir$x
  args$w = envir$w
  args$k = method$nClusters
  args$addintercept.fixed = FALSE
  args$addintercept.random = FALSE
  args$verb = canShow(verbose, 'fine')

  model = do.call(regmixEM.mixed, args)
  model$fixed = envir$fixed
  model$random = envir$random

  new('clModelMixtoolsGMM',
      method=method,
      data=data,
      model=model,
      clusterNames=make.clusterNames(method$nClusters))
})
