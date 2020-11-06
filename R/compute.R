#' @title Compute the id-specific postprob matrix from a given observation-level postprob matrix
#' @importFrom matrixStats rowLogSumExps
#' @param mat A posterior probability `matrix` at the observation level.
#' @param rowIds The row trajectory identifier `integer` vector.
#' @keywords internal
postProbFromObs = function(mat, rowIds) {
  assert_that(
    is.matrix(mat),
    noNA(mat),
    nrow(mat) == length(rowIds),
    noNA(rowIds)
  )

  logPp = log(mat) %>%
    rowsum(rowIds)

  sweep(logPp, 1, rowLogSumExps(logPp)) %>% exp
}
