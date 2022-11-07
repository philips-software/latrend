#' @include model.R
#' @export
#' @name lcApproxModel-class
#' @rdname lcApproxModel-class
#' @aliases lcApproxModel
#' @title lcApproxModel class
#' @description approx models have defined cluster trajectories at fixed moments in time, which should be interpolated
#' For a correct implementation, `lcApproxModel` requires the extending class to implement `clusterTrajectories(at=NULL)`
#' to return the fixed cluster trajectories
setClass('lcApproxModel', contains = 'lcModel')

#' @export
#' @rdname lcApproxModel-class
#' @inheritParams fitted.lcModel
fitted.lcApproxModel = function(object, ..., clusters = trajectoryAssignments(object)) {
  newdata = subset(
    model.data(object),
    select = c(idVariable(object), timeVariable(object), responseVariable(object))
  )
  pred = predict(object, newdata = newdata, useCluster = FALSE)
  transformFitted(pred, model = object, clusters = clusters)
}

#. predictForCluster ####
#' @rdname lcApproxModel-class
#' @inheritParams predictForCluster
#' @param approxFun Function to interpolate between measurement moments, \link[stats]{approx}() by default.
setMethod('predictForCluster', 'lcApproxModel',
  function(object, newdata, cluster, what = 'mu', approxFun = approx, ...) {
  assert_that(is.function(approxFun))

  clusTrajs = clusterTrajectories(object, at = numeric(), what = what, approxFun = approxFun, ...) %>%
    as.data.table() %>%
    .[Cluster == cluster]

  time = timeVariable(object)
  resp = responseVariable(object)
  clusTimes = clusTrajs[[time]]
  newtimes = newdata[[time]]

  # check if we need to do any interpolation
  if (all(newtimes %in% clusTimes)) {
    pred = clusTrajs[match(newtimes, get(time)), get(resp)]
    return(pred)
  }

  if (sum(is.finite(clusTrajs[[resp]])) < 2) {
    warning(
      sprintf(
        'Cannot interpolate cluster trajectory of cluster %s: need at least two non-NA cluster trajectory values',
        cluster
      )
    )
    return(rep(NaN, length(newtimes)))
  }

  dtpred = clusTrajs[,
    lapply(.SD, function(y) approxFun(x = get(time), y = y, xout = newtimes)$y),
    keyby = Cluster, .SDcols = -c(time)
  ]

  dtpred[[resp]]
})
