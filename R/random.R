localRNG = function(seed = NULL, expr) {
  if (is.null(seed)) {
    force(expr)
    return(invisible())
  } else {
    if (exists('.Random.seed')) {
      prevSeed = .Random.seed
      set.seed(seed)
      force(expr)
      .Random.seed = prevSeed
    }
    else {
      set.seed(seed)
      force(expr)
    }
  }
}

#' @export
#' @title Sample an index of a vector weighted by the elements
#' @description Returns a random index, weighted by the element magnitudes. This function is intended to be used as an optional strategy for [trajectoryAssignments], resulting in randomly sampled cluster membership.
#' @param x A positive `numeric vector`.
#' @return An `integer` giving the index of the sampled element.
#' @examples
#' x = c(.01, .69, .3)
#' which.weight(x) #1, 2, or 3
which.weight = function(x) {
  sample.int(length(x), size = 1, prob = x)
}
