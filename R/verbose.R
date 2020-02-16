verboseLevels = list(info=0, fine=-10, finest=-99, debug=-100)

#' @importFrom R.utils Verbose cat cat.Verbose print.Verbose header ruler pushState popState enter exit warnings getThreshold
as.Verbose = function(x) {
  if(inherits(x, 'Verbose')) {
    return(x)
  }

  assert_that(is.scalar(x))

  if(is.null(x)) {
    Verbose()
  } else if(is.logical(x)) {
    Verbose(threshold=-x)
  } else if(is.character(x)) {
    assert_that(x %in% names(verboseLevels))
    Verbose(threshold=verboseLevels[[x]] - 1)
  } else if(is.numeric(x)) {
    Verbose(threshold=x)
  } else {
    stop('unsupported verbose input')
  }
}

canShow = function(verbose, level) {
  getThreshold(verbose) < getThreshold(as.Verbose(level))
}