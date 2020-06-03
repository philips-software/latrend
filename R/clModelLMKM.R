#' @include clModel.R
setClass('clModelLMKM', representation(coefNames='character'), contains='clModel')

coef.clModelLMKM = function(object, cluster=NULL) {
  coefmat = t(object@model$centers)
  colnames(coefmat) = clusterNames(object)
  rownames(coefmat) = object@coefNames

  if(is.null(cluster)) {
    return(coefmat)
  } else {
    assert_that(is.count(cluster) || is.character(cluster) && cluster %in% clusterNames(object))
    coefmat[, cluster, drop=FALSE]
  }
}

#. converged ####
setMethod('converged', signature('clModelLMKM'), function(object) {
  if(nClusters(object) == 1) {
    TRUE
  }
  else {
    !object@model$ifault
  }
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
    newdata = model.data(object) %>%
      subset(select=getCovariates(formula(object)))
  }
  newdata = as.data.table(newdata)

  if(nrow(newdata) == 0) {
    # predict.lm cannot handle empty data.frame, so return early
    return(transformPredict(object, pred=NULL, newdata=newdata))
  }

  # create ref lm
  method = getMethod(object)
  lmArgs = method[lm, expand=FALSE]
  data = model.data(object)
  refdata = data[get(method$id) == first(get(method$id))][1:min(10, .N)]
  refmod = do.call(lm, c(lmArgs, data=list(refdata)))

  # construct lm per cluster
  clusmods = lapply(clusterNames(object), function(clusName) {
    clusmod = refmod
    clusmod$coefficients = coef(object, cluster=clusName)
    return(clusmod)
  })

  predfun = function(clusmod, clusdata) {
    out = predict(clusmod, newdata=clusdata, ...)
    if(is.numeric(out)) {
      list(fit=out)
    } else {
      out
    }
  }

  if(has_name(newdata, 'Cluster')) {
    clusdataList = split(newdata, by='Cluster', sorted=TRUE)
  } else {
    clusdataList = list(newdata)
  }

  dtpred = mapply(predfun, clusmods, clusdataList, SIMPLIFY=FALSE) %>%
    rbindlist(idcol='Cluster') %>%
    .[, Cluster := factor(Cluster, labels=clusterNames(object))]
  setnames(dtpred, 'fit', 'Fit')
  transformPredict(object, dtpred, newdata=newdata)
}