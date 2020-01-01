#' @include clMethod.R
setClass('clMethodCustom', contains='clMethod')

#' @export
#' @title Specify a custom method based on a model function
#' @param fun The cluster function with signature (method, data).
#' @param center Optional method for computing the longitudinal cluster centers.
#' @param time Time variable.
#' @param id Strata variable.
#' @examples
#' # Stratification based on the mean response level
#' clusfun = function(method, data) {
#'    clusters = data[, mean(Value) > 2, by=Id] %>%
#'        factor(levels=c(F,T), labels=c('Low', 'High'))
#'    list(clusters=clusters)
#'    clModelCustom(clusters=clusters)
#' }
#' method = clMethodCustom(fun=clusfun)
#' model = cluslong(method, testLongData)
#' summary(model)
clMethodCustom = function(fun,
                       center=meanNA,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       name='custom') {
  object = new('clMethodCustom', call=match.call.defaults())

  if(getOption('cluslong.checkArgs')) {
    checkArgs(object, envir=parent.frame())
  }
  return(object)
}


setMethod('checkArgs', signature('clMethodCustom'), function(object, envir) {
  environment(object) = envir
  assert_that(all(formalArgs(clMethodCustom) %in% names(getCall(object))), msg='clMethod object is missing required arguments')

  if(isArgDefined(object, 'fun')) {
    assert_that(is.function(object$fun))
  }

  if(isArgDefined(object, 'center')) {
    assert_that(is.function(object$center))
  }
})

setMethod('getName', signature('clMethodCustom'), function(object) {
  if(isArgDefined(object, 'name') && !is.null(object$name)) {
    return(object$name)
  }

  if(isArgDefined(object, 'fun')) {
    fun = object[['fun', eval=FALSE]]
    if(is.name(fun)) {
      return(paste('custom function ', fun))
    }
  }

  return('custom function')
})

setMethod('getName0', signature('clMethodCustom'), function(object) 'custom')


setMethod('prepare', signature('clMethodCustom'), function(method, data, control) {

})

setMethod('fit', signature('clMethodCustom'), function(method, data, control, prepEnv) {
  e = new.env()
  args = as.list(method)
  args$data = data

  startTime = Sys.time()
  e$model = do.call(method$fun, args)
  e$runTime = as.numeric(Sys.time() - startTime)
  return(e)
})

setMethod('finalize', signature('clMethodCustom'), function(method, data, control, fitEnv) {
  # check output
  model = fitEnv$model
  assert_that(is.clModel(model))
  model@method = method
  return(model)
})
