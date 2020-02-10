#' @include clMethod.R
setClass('clMethodMixtoolsGMM', contains='clMethod')

#' @export
#' @importFrom mixtools regmixEM.mixed
#' @title Specify mixed mixture regression model using mixtools
#' @inheritParams mixtools::regmixEM.mixed
#' @examples
#' method = clMethodMixtoolsGMM(Value ~ Time + (Time | Id),
#'                      time='Time',
#'                      id='Id', nClusters=3)
#' @family clMethod classes
clMethodMixtoolsGMM = function(formula=Value ~ 1 + (Time + I(Time^2) | Id),
                               time=getOption('cluslong.time'),
                               id=getOption('cluslong.id'),
                               nClusters=2,
                               arb.sigma=FALSE,
                               arb.R=FALSE,
                               ar.1=FALSE,
                               maxit=500,
                               epsilon=1e-8,
                               ...) {
  new('clMethodMixtoolsGMM', call=match.call.defaults())
}

setMethod('getName', signature('clMethodMixtoolsGMM'), function(object) 'growth mixture modeling using mixtools')

setMethod('getName0', signature('clMethodMixtoolsGMM'), function(object) 'gmm')


setMethod('prepare', signature('clMethodMixtoolsGMM'), function(method, data) {
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
setMethod('fit', signature('clMethodMixtoolsGMM'), function(method, data, prepEnv) {
  e = new.env(parent=prepEnv)

  args = as.list(method)
  args$y = prepEnv$y
  args$x = prepEnv$x
  args$w = prepEnv$w
  args$k = method$nClusters
  args$addintercept.fixed = FALSE
  args$addintercept.random = FALSE
  args$verb = canShowModelOutput('FINE')
  args[setdiff(names(args), formalArgs(regmixEM.mixed))] = NULL #remove undefined arguments

  startTime = Sys.time()

  model = do.call(regmixEM.mixed, args)
  model$fixed = prepEnv$fixed
  model$random = prepEnv$random

  e$runTime = as.numeric(Sys.time() - startTime)

  e$model = model
  return(e)
})

setMethod('finalize', signature('clMethodMixtoolsGMM'), function(method, data, fitEnv) {
  model = new('clModelMixtoolsGMM',
              method=method,
              data=data,
              model=fitEnv$model,
              clusterNames=make.clusterNames(method$nClusters))
  return(model)
})