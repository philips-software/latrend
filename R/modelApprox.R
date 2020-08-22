#' @include model.R
#' @export
#' @name lcApproxModel-class
#' @rdname lcApproxModel-class
#' @aliases lcApproxModel
#' @title lcApproxModel class
#' @description approx models have defined cluster trajectories at fixed moments in time, which should be interpolated
#' For a correct implementation, lcApproxModel requires the extending class to implement clusterTrajectories(at=NULL)
#' to return the fixed cluster trajectories
setClass('lcApproxModel', contains = 'lcModel')

#' @export
#' @rdname lcApproxModel-class
#' @inheritParams fitted.lcModel
fitted.lcApproxModel = function(object, ..., clusters = clusterAssignments(object)) {
  times = time(object)
  newdata = data.table(Id = ids(object) %>% rep(each = length(times)),
                       Time = times) %>%
    setnames('Id', idVariable(object)) %>%
    setnames('Time', timeVariable(object))

  predict(object, newdata = newdata) %>%
    transformFitted(model = object, clusters = clusters)
}

#' @export
#' @rdname lcApproxModel-class
#' @inheritParams predict.lcModel
#' @param approxFun The interpolation function to use for time points not in the feature set.
predict.lcApproxModel = function(object, ...,
                                 newdata = NULL,
                                 what = 'mu',
                                 approxFun = approx) {
  assert_that(is.newdata(newdata), is.function(approxFun))

  if (is.null(newdata)) {
    predMat = fitted(object, clusters = NULL)
    transformPredict(pred = predMat,
                     model = object,
                     newdata = newdata)
  } else {
    time = timeVariable(object)
    resp = responseVariable(object)
    assert_that(has_name(newdata, time))

    # compute cluster trajectories
    clusTrajs = clusterTrajectories(object, at = NULL, what = what) %>% as.data.table()
    assert_that(
      is.data.frame(clusTrajs),
      has_name(clusTrajs, 'Cluster'),
      has_name(clusTrajs, time),
      has_name(clusTrajs, resp)
    )

    if (has_name(newdata, 'Cluster')) {
      assert_that(any(unique(clusTrajs$Cluster) %in% clusterNames(object)), msg =
                    'invalid clusterTrajectories output: Cluster column has wrong names for the clusters')

      # generate newdata prediction for the specified cluster(s)
      newclusdata = as.data.table(newdata)

      # to be simplified..
      dtcluspred = clusTrajs[Cluster %in% unique(newclusdata$Cluster),
                             as.list(newclusdata[Cluster == .BY[[1]], -'Cluster']) %>%
                               c(list(Fit = approxFun(
                                 x = get(time),
                                 y = get(resp),
                                 xout = newclusdata[Cluster == .BY[[1]], get(time)]
                               )$y)),
                             by = Cluster]
    } else {
      # generate newdata prediction per cluster
      newtimes = newdata[[timeVariable(object)]]
      dtcluspred = clusTrajs[, as.list(newdata) %>% c(list(Fit = approxFun(
        x = get(time),
        y = get(resp),
        xout = newtimes
      )$y)), by = Cluster]
      assert_that(nrow(dtcluspred) <= nrow(newdata) * nClusters(object))
    }

    assert_that(nrow(dtcluspred) > 0 || nrow(newdata) == 0)
    transformPredict(pred = dtcluspred,
                     model = object,
                     newdata = newdata)
  }
}
