#' @include clModel.R
setClass('clModelLLPA', contains='clModel')


#' @export
#' @rdname predict.clModel
#' @inheritParams predict.clModel
#' @param approxFun The interpolation function to use for time points not in the feature set.
predict.clModelLLPA = function(object, newdata=NULL, what='mu', approxFun=approx) {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu', msg='only what="mu" is supported')
  assert_that(is.function(approxFun))

  # compute cluster trajectories
  trajMat = object@model$parameters$mean

  if(is.null(newdata)) {
    return(fitted(object))
  } else {
    assert_that(has_name(newdata, timeVariable(object)))
    newtimes = newdata[[timeVariable(object)]]
    predMat = apply(trajMat, 2, function(y) approxFun(x=time(object), y=y, xout=newtimes)$y)
  }

  transformPredict(object, predMat, newdata=newdata)
}


#' @export
fitted.clModelLLPA = function(object, clusters=clusterAssignments(object)) {
  times = time(object)
  newdata = data.table(Id=rep(modelIds(object), each=length(times)),
                       Cluster=rep(clusters, each=length(times)),
                       Time=times) %>%
    setnames('Id', idVariable(object)) %>%
    setnames('Time', timeVariable(object))
  predict(object, newdata=newdata)
}


# . postprob ####
setMethod('postprob', signature('clModelLLPA'), function(object) {
  pp = object@model$z
  colnames(pp) = clusterNames(object)
  return(pp)
})


#. predictPostprob ####
setMethod('predictPostprob', signature('clModelLLPA'), function(object, newdata=NULL) {
  if(is.null(newdata)) {
    postprob(object)
  } else {
    stop('not implemented')
    pp = predict(object@model, newdata=newdata)$z
  }
})


# . converged ####
setMethod('converged', signature('clModelLLPA'), function(object) {
  TRUE
})


# . modelIds ####
setMethod('modelIds', signature('clModelLLPA'), function(object) {
  object@model$data %>% rownames
})

# . modelTimes ####
setMethod('modelTimes', signature('clModelLLPA'), function(object) {
  object@model$time
})

# . modelData ####
setMethod('modelData', signature('clModelLLPA'), function(object) {
  resp = responseVariable(object)
  id = idVariable(object)
  time = timeVariable(object)
  times = modelTimes(object)

  data = data.table(Id=rep(modelIds(object), each=length(times)),
                    Time=times,
                    Value=as.numeric(object@model$data)) %>%
    setnames(c(id, time, resp))
  return(data)
})
