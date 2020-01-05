#' @title Select the specified column per row
#' @keywords internal
rowColumns = function(x, i) {
  assert_that(is.matrix(x))
  assert_that(length(i) == nrow(x))
  x[cbind(1:nrow(x), i)]
}