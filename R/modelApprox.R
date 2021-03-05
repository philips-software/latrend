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
  times = time(object)
  newdata = data.table(Id = ids(object) %>% rep(each = length(times)),
                       Time = times) %>%
    setnames('Id', idVariable(object)) %>%
    setnames('Time', timeVariable(object))

  predict(object, newdata = newdata) %>%
    transformFitted(model = object, clusters = clusters)
}

#. predictForCluster ####
#' @rdname lcApproxModel-class
#' @inheritParams predictForCluster
#' @param approxFun Function to interpolate between measurement moments, \link[stats]{approx}() by default.
setMethod('predictForCluster', signature('lcApproxModel'),
  function(object, newdata, cluster, what = 'mu', approxFun = approx, ...) {
  assert_that(is.function(approxFun))

  clusTrajs = clusterTrajectories(object, at = NULL, what = what, approxFun = approxFun, ...) %>%
    as.data.table() %>%
    .[Cluster == cluster]

  time = timeVariable(object)
  resp = responseVariable(object)
  newtimes = newdata[[time]]

  dtpred = clusTrajs[, lapply(.SD, function(y)
        approxFun(
          x = get(time),
          y = y,
          xout = newtimes)$y),
    keyby = Cluster, .SDcols = -c(time)]

  dtpred[[resp]]
})
