#' @include clModel.R
setClass('clModelLMKM', contains='clModel')

#. converged ####
setMethod('converged', signature('clModelLMKM'), function(object) {
  !object@model$ifault
})

#. postprob ####
setMethod('postprob', signature('clModelLMKM'), function(object) {
  k = nrow(object@model$centers)
  postprobFromAssignments(object@model$cluster, k)
})

#' @export
#' @inheritDotParams stats::predict.lm
predict.clModelLMKM = function(object, newdata=NULL, what='mu', ...) {
  assert_that(is.newdata(newdata))
  assert_that(what == 'mu')

  if(is.null(newdata)) {
    newdata = model.data(object)
  }

  # construct lm object per cluster
  lm = do.call(lm, c(lmArgs, data=data[Id == first(Id)]))

  browser()
  f = formula(object) %>% dropResponse()
  if(has_name(newdata, 'Cluster')) {
    X = model.matrix(f, data=newdata) %>%
      cbind(Cluster=newdata$Cluster)
  } else {
    x = replicate(nClusters(object), data.frame(model.matrix(f, data=newdata)), simplify=FALSE) %>%
      rbindlist(idcol='Cluster') %>%
      .[, Cluster := factor(Cluster, labels=clusterNames(object))]
  }

}