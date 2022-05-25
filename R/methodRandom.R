#' @include method.R
setClass('lcMethodRandom', contains = 'lcMethod')

setValidity('lcMethodRandom', function(object) {
  assert_that(
    has_lcMethod_args(object, formalArgs(lcMethodRandom))
  )

  if (isArgDefined(object, 'alpha')) {
    assert_that(
      is.numeric(object$alpha),
      all(object$alpha >= 0),
      all(is.finite(object$alpha))
    )
  }

  if (isArgDefined(object, 'center')) {
    assert_that(is.function(object$center))
  }
})


#' @export
#' @title Specify a random-partitioning method
#' @description Creates a model with random cluster assignments according to the random cluster proportions drawn from a Dirichlet distribution.
#' @inheritParams lcMethodCustom
#' @param nClusters The number of clusters.
#' @param alpha The Dirichlet parameters. Either `scalar` or of length `nClusters`. The higher alpha, the more uniform the clusters will be.
#' @param ... Additional arguments, such as the seed.
#' @examples
#' data(latrendData)
#' method <- lcMethodRandom(response = "Y", id = "Id", time = "Time")
#' model <- latrend(method, latrendData)
#'
#' # uniform clusters
#' method <- lcMethodRandom(
#'   alpha = 1e3,
#'   nClusters = 3,
#'   response = "Y",
#'   id = "Id",
#'   time = "Time"
#' )
#'
#' # single large cluster
#' method <- lcMethodRandom(
#'   alpha = c(100, 1, 1, 1),
#'   nClusters = 4,
#'   response = "Y",
#'   id = "Id",
#'   time = "Time"
#' )
#' @family lcMethod implementations
#' @references
#' \insertRef{frigyik2010introduction}{latrend}
lcMethodRandom = function(
  response,
  alpha = 10,
  center = meanNA,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  nClusters = 2,
  name = 'random',
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodRandom'
  do.call(new, as.list(mc))
}

#' @rdname interface-custom
setMethod('getArgumentDefaults', signature('lcMethodRandom'), function(object) {
  c(
    formals(lcMethodRandom),
    callNextMethod()
  )
})

#' @rdname interface-custom
setMethod('getName', signature('lcMethodRandom'), function(object) 'random')

#' @rdname interface-custom
setMethod('getShortName', signature('lcMethodRandom'), function(object) 'rand')

#' @rdname interface-custom
setMethod('fit', signature('lcMethodRandom'), function(method, data, envir, verbose, ...) {
  nIds = uniqueN(data[[idVariable(method)]])
  assert_that(nIds > 0, msg = 'cannot fit to data with nIds = 0')

  # generate cluster proportions
  y = stats::rgamma(method$nClusters, method$alpha)
  clusProps = y / sum(y)

  propSeq = rep(seq_len(method$nClusters), ceiling(clusProps * nIds))

  clusAssign = sample(propSeq)[seq_len(nIds)]

  model = lcModelPartition(
    data = data,
    response = method$response,
    id = method$id,
    time = method$time,
    trajectoryAssignments = clusAssign,
    center = method$center,
    method = method
  )

  model
})
