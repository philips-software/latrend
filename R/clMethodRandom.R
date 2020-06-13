#' @include clMethod.R
setClass('clMethodRandom', contains='clMethod')

setValidity('clMethodRandom', function(object) {
  assert_that(has_clMethod_args(object, formalArgs(clMethodRandom)))

  if(isArgDefined(object, 'alpha')) {
    assert_that(is.numeric(object$alpha), all(object$alpha >= 0), all(is.finite(object$alpha)))
  }

  if(isArgDefined(object, 'center')) {
    assert_that(is.function(object$center))
  }
})


#' @export
#' @title Specify a random-partitioning method
#' @description Creates a model with random assignments according to the cluster proportions drawn from a Dirichlet distribution.
#' @inheritParams clMethodCustom
#' @param alpha The Dirichlet parameters. Either `scalar` or of length `nClusters`. The higher alpha, the more uniform the clusters will be.
#' @examples
#' m = clMethodRandom()
#'
#' # uniform clusters
#' m = clMethodRandom(alpha=1e3, nClusters=3)
#'
#' # single large cluster
#' m = clMethodRandom(alpha=c(100, 1, 1, 1), nClusters=4)
#' @family clMethod implementations
clMethodRandom = function(alpha=10,
                          center=meanNA,
                          response=getOption('cluslong.response'),
                          time=getOption('cluslong.time'),
                          id=getOption('cluslong.id'),
                          nClusters=2,
                          name='random') {
  .clMethod.call('clMethodRandom', call=match.call.defaults())
}

setMethod('getName', signature('clMethodRandom'), function(object) 'random')

setMethod('getShortName', signature('clMethodRandom'), function(object) 'rand')

setMethod('fit', signature('clMethodRandom'), function(method, data, envir, verbose, ...) {
  nIds = uniqueN(data[[idVariable(method)]])

  # generate cluster proportions
  y = rgamma(method$nClusters, method$alpha)
  clusProps = y / sum(y)

  propSeq = rep(1:method$nClusters, ceiling(clusProps * nIds))

  clusAssign = sample(propSeq)[1:nIds]

  clModelCustom(method=method,
                data=data,
                clusterAssignments=clusAssign,
                clusterTrajectories=method$center,
                converged=TRUE)
})
