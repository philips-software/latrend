#' @include clModel.R
setClass('clModelGLMKM', contains='clModel')

#' @export
fitted.clModelGLMKM = function(object, clusters=clusterAssignments(object)) {

}

#' @export
predict.clModelGLMKM = function(object, newdata=NULL, what='mu') {

}

setMethod('postprob', signature('clModelGLMKM'), function(object) {

})

setMethod('converged', signature('clModelGLMKM'), function(object) {
  !object@model$ifault
})
