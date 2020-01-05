#' @title Select the specified column per row
#' @keywords internal
rowColumns = function(x, i) {
  assert_that(is.matrix(x) && all(is.finite(i)))
  assert_that(length(i) == nrow(x) && max(i) <= nrow(x) && min(i) >= 1)
  x[cbind(1:nrow(x), i)]
}