#' @include model.R
setClass('lcModelLMKM', representation(coefNames = 'character'), contains = 'lcModel')


#. predictForCluster ####
#' @rdname interface-featureBased
#' @inheritParams predictForCluster
#' @inheritDotParams stats::predict.lm
setMethod('predictForCluster', signature('lcModelLMKM'), function(
  object, newdata, cluster, what = 'mu', ...)
{
  assert_that(what == 'mu')

  if (nrow(newdata) == 0) {
    # predict.lm cannot handle empty data.frame, so return early
    return(numeric())
  }

  # create ref lm
  method = getLcMethod(object)
  id = idVariable(method)
  lmArgs = as.list(method, args = lm)
  data = model.data(object) %>% as.data.table()
  # take some data just to fit a valid lm as a template
  refdata = data[get(id) == first(get(id))][1:min(10, .N)]
  clusmod = do.call(lm, c(lmArgs, data = list(refdata)))

  # construct lm
  clusmod$coefficients = coef(object, cluster = cluster)

  predfun = function(clusmod, clusdata) {
    out = predict(clusmod, newdata = clusdata, ...)
    if (is.numeric(out)) {
      list(fit = out)
    } else {
      out
    }
  }

  preds = predict(clusmod, newdata = newdata, ...)

  preds
})


#' @export
#' @rdname interface-featureBased
#' @param cluster The cluster name.
coef.lcModelLMKM = function(object, ..., cluster = NULL) {
  coefmat = t(object@model$centers)
  colnames(coefmat) = clusterNames(object)
  rownames(coefmat) = object@coefNames

  if (is.null(cluster)) {
    return(coefmat)
  } else {
    assert_that(is.count(cluster) ||
                  is.character(cluster) && cluster %in% clusterNames(object))
    coefmat[, cluster, drop = FALSE]
  }
}


#. converged ####
#' @rdname interface-featureBased
setMethod('converged', signature('lcModelLMKM'), function(object, ...) {
  if (nClusters(object) == 1) {
    TRUE
  }
  else {
    not(object@model$ifault)
  }
})


#. postprob ####
#' @rdname interface-featureBased
setMethod('postprob', signature('lcModelLMKM'), function(object, ...) {
  k = nrow(object@model$centers)
  postprobFromAssignments(object@model$cluster, k)
})
