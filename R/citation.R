tmpCiteEnv = new.env(parent = emptyenv())

.suggestCiteMethod = function(x, optional = TRUE) {
  assert_that(
    is.lcMethod(x) || is.lcModel(x),
    is.flag(optional)
  )

  cite = getCitation(x)
  citeName = paste0('latrend.cited.', class(x))
  skip = getOption(citeName, default = FALSE)
  assert_that(
    is.flag(skip),
    msg = sprintf('Invalid value for option "%s": should be a flag (T/F)', citeName)
  )

  if (length(cite) == 0 || exists(citeName, envir = tmpCiteEnv) || optional && skip) {
    return()
  }

  message(
    sprintf(
      'Attribution notice: %1$s makes use of external R package(s):
  - Run `getCitation(model)` to view how to cite the external package(s) when reporting results.
  - To disable this notice, run `options(%2$s = TRUE)`.',
      class(x),
      citeName
    )
  )

  assign(citeName, value = TRUE, envir = tmpCiteEnv)
}

.clearCited = function() {
  tmpCiteEnv = new.env(parent = emptyenv())
}
