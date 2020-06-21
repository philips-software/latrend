localRNG = function(seed = NULL, expr) {
  if (is.null(seed)) {
    force(expr)
    return(invisible())
  } else {
    prevSeed = .Random.seed
    set.seed(seed)
    force(expr)
    .Random.seed = prevSeed
  }
}
