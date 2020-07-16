#' @include model.R
setClass('lcModelLongclust', contains='lcModel')


#' @export
predict.lcModelLongclust = function(object, newdata=NULL, what='mu', approxFun=approx) {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu')
  assert_that(is.function(approxFun))

  # compute cluster trajectories
  trajMat = object@model$mubest
  assert_that(is.matrix(trajMat), nrow(trajMat) > 0, msg='empty estimate for mu. model probably did not converge')

  if(is.null(newdata)) {
    predMat = fitted(object, clusters=NULL)
  } else {
    assert_that(has_name(newdata, timeVariable(object)))
    newtimes = newdata[[timeVariable(object)]]
    predMat = apply(trajMat, 1, function(y) approxFun(x=time(object), y=y, xout=newtimes)$y)
  }

  transformPredict(pred = predMat, model = object, newdata = newdata)
}


#' @export
fitted.lcModelLongclust = function(object, clusters=clusterAssignments(object)) {
  times = time(object)
  newdata = data.table(Id=rep(ids(object), each=length(times)),
                       Time=times) %>%
    setnames('Id', idVariable(object)) %>%
    setnames('Time', timeVariable(object))
  predict(object, newdata=newdata) %>%
    transformFitted(model = object, clusters)
}


setMethod('postprob', signature('lcModelLongclust'), function(object) {
  pp = object@model$zbest
  colnames(pp) = clusterNames(object)
  return(pp)
})


setMethod('converged', signature('lcModelLongclust'), function(object) {
  object@model$Gbest > 0
})


#' @export
logLik.lcModelLongclust = function(object) {
  logLiks = -object@model$llres
  if(length(logLiks) == 1) {
    ll = logLiks
  } else {
    # determine best model based on criteria
    bestIdx = switch(tolower(getLcMethod(object)$criteria),
           bic={which.max(object@model$bicres)},
           icl={which.max(object@model$iclres)})
    ll = logLiks[bestIdx]
  }
  N = nIds(object)
  attr(ll, 'nobs') = N
  class(ll) = 'logLik'
  return(ll)
}

#' @export
BIC.lcModelLongclust = function(object) {
  bics = -object@model$bicres
  if(length(bics) == 1) {
    bics
  } else {
    # determine best model based on criteria
    bestIdx = switch(tolower(getLcMethod(object)$criteria),
                     bic={which.max(object@model$bicres)},
                     icl={which.max(object@model$iclres)})
    bics[bestIdx]
  }
}
