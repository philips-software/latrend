#' @include clModel.R
setClass('clModelMclustLLPA', contains = 'clModel')


#' @export
predict.clModelMclustLLPA = function(object,
                                     newdata = NULL,
                                     what = 'mu',
                                     approxFun = approx) {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu', msg = 'only what="mu" is supported')
  assert_that(is.function(approxFun))

  # compute cluster trajectories
  trajMat = object@model$parameters$mean

  if (is.null(newdata)) {
    predMat = fitted(object, clusters = NULL)
  } else {
    assert_that(has_name(newdata, timeVariable(object)))
    newtimes = newdata[[timeVariable(object)]]
    predMat = apply(trajMat, 2, function(y)
      approxFun(
        x = time(object),
        y = y,
        xout = newtimes
      )$y)
  }

  transformPredict(pred = predMat,
                   model = object,
                   newdata = newdata)
}


#' @export
fitted.clModelMclustLLPA = function(object, clusters = clusterAssignments(object)) {
  times = time(object)
  newdata = data.table(Id = rep(ids(object), each = length(times)),
                       Time = times) %>%
    setnames('Id', idVariable(object)) %>%
    setnames('Time', timeVariable(object))
  predict(object, newdata = newdata) %>%
    transformFitted(model = object, clusters)
}


# . postprob ####
setMethod('postprob', signature('clModelMclustLLPA'), function(object) {
  pp = object@model$z
  colnames(pp) = clusterNames(object)
  return(pp)
})


#. predictPostprob ####
setMethod('predictPostprob', signature('clModelMclustLLPA'), function(object, newdata =
                                                                        NULL) {
  if (is.null(newdata)) {
    postprob(object)
  } else {
    stop('not implemented')
    pp = predict(object@model, newdata = newdata)$z
  }
})


# . converged ####
setMethod('converged', signature('clModelMclustLLPA'), function(object) {
  TRUE
})
