#' @include model.R
setClass('lcModelMclustLLPA', contains = 'lcModel')


#' @export
#' @rdname interface-mclust
#' @inheritParams predict.lcApproxModel
predict.lcModelMclustLLPA = function(object,
                                     ...,
                                     newdata = NULL,
                                     what = 'mu',
                                     approxFun = approx) {
  assert_that(is.newdata(newdata),
    is.function(approxFun))
  assert_that(what == 'mu', msg = 'only what="mu" is supported')

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
#' @rdname interface-mclust
#' @inheritParams fitted.lcApproxModel
fitted.lcModelMclustLLPA = function(object, ..., clusters = trajectoryAssignments(object)) {
  times = time(object)
  newdata = data.table(Id = rep(ids(object), each = length(times)),
                       Time = times) %>%
    setnames('Id', idVariable(object)) %>%
    setnames('Time', timeVariable(object))
  predict(object, newdata = newdata) %>%
    transformFitted(model = object, clusters)
}


# . postprob ####
#' @rdname interface-mclust
setMethod('postprob', signature('lcModelMclustLLPA'), function(object, ...) {
  pp = object@model$z
  colnames(pp) = clusterNames(object)
  return(pp)
})


#. predictPostprob ####
#' @rdname interface-mclust
setMethod('predictPostprob', signature('lcModelMclustLLPA'),
  function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    callNextMethod()
  } else {
    predict(object@model, newdata = newdata)$z
  }
})


# . converged ####
#' @rdname interface-mclust
setMethod('converged', signature('lcModelMclustLLPA'), function(object, ...) {
  TRUE
})
