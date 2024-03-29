#' @include model.R
setClass('lcModelMclustLLPA', contains = 'lcApproxModel')


#. clusterTrajectories ####
#' @rdname interface-mclust
#' @inheritParams clusterTrajectories
#' @inheritParams predictForCluster
setMethod('clusterTrajectories', 'lcModelMclustLLPA', function(object, at = time(object), ...) {
  if (length(at) == 0) {
    trajMat = object@model$parameters$mean
    assert_that(
      is.matrix(trajMat),
      nrow(trajMat) > 0,
      msg='empty estimate for mean. model probably did not converge'
    )

    dtclus = tsframe(
      t(trajMat),
      times = time(object),
      id = 'Cluster',
      ids = clusterNames(object),
      time = timeVariable(object),
      response = responseVariable(object))
  } else {
    callNextMethod()
  }
})


# . postprob ####
#' @rdname interface-mclust
setMethod('postprob', 'lcModelMclustLLPA', function(object, ...) {
  pp = object@model$z
  colnames(pp) = clusterNames(object)

  pp
})


#. predictPostprob ####
#' @rdname interface-mclust
setMethod('predictPostprob', 'lcModelMclustLLPA',
  function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    callNextMethod()
  } else {
    predict(object@model, newdata = newdata)$z
  }
})


# . converged ####
#' @rdname interface-mclust
setMethod('converged', 'lcModelMclustLLPA', function(object, ...) {
  TRUE
})
