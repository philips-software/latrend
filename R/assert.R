is.named = function(x) {
  !is.null(names(x))
}

is.newdata = function(x) {
  is.null(x) || is.list(x) && is.named(x)
}