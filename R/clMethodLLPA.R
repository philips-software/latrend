setClass('clMethodLLPA', contains='clMethod')

setValidity('clMethodLLPA', function(object) {
  call = getCall(object)
  assert_that(hasSingleResponse(object$formula))

  assert_that(all(formalArgs(clMethodLLPA) %in% names(call)), msg='clMethod object is missing required arguments')

  assert_that(is.wholeNumber(object$nClusters))
})

#' @export
#' @import mclust
#' @title Longitudinal latent profile analysis
#' @description Latent profile analysis or finite Gaussian mixture modeling.
#' @param formula Formula. Covariates are not supported.
#' @param time Time variable.
#' @param id Strata variable.
#' @param nClusters Number of clusters.
#' @inheritParams mclust::Mclust
#' @examples
#' method = clMethodLLPA(Measurement ~ 1,
#'                      time='Assessment',
#'                      id='Id', nClusters=3)
clMethodLLPA = function(formula=Value ~ 1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       modelNames=NULL,
                       prior=NULL,
                       initialization=NULL) {
  new('clMethodLLPA', call=match.call.defaults())
}

setMethod('getName', signature('clMethodLLPA'), function(object) 'longitudinal latent profile analysis')

setMethod('getName0', signature('clMethodLLPA'), function(object) 'llpa')

setMethod('prepare', signature('clMethodLLPA'), function(method, data, control) {
  e = new.env()

  valueColumn = formula(method) %>% getResponse
  assert_that(!anyNA(data[[valueColumn]]), msg='data contains missing values')

  # Data
  logfine('Reshaping data...')
  wideFrame = dcast(data, get(method$id) ~ get(method$time), value.var=valueColumn)
  e$data = as.matrix(wideFrame[, -'method']) %>%
    set_rownames(wideFrame$method)

  return(e)
})

setMethod('fit', signature('clMethodLLPA'), function(method, data, control, prepEnv) {
  e = new.env(parent=prepEnv)
  args = as.list(method)
  args$data = prepEnv$data
  args$G = method$nClusters

  startTime = Sys.time()
  model = do.call(Mclust, args)
  model$time = unique(data[[method$time]]) %>% sort
  e$model = model
  e$runTime = as.numeric(Sys.time() - startTime)

  return(e)
})

setMethod('finalize', signature('clMethodLLPA'), function(method, data, control, fitEnv) {
  model = new('clModelLLPA',
              method=method,
              model=fitEnv$model,
              clusterNames=genClusNames(method$nClusters))
  return(model)
})
