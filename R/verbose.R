verboseLevels = list(
  info = 0,
  fine = -10,
  finest = -99,
  debug = -100
)

#' @importFrom R.utils Verbose cat cat.Verbose print.Verbose warnings.Verbose header ruler pushState popState enter exit warnings getThreshold
as.Verbose = function(x) {
  if (inherits(x, 'Verbose')) {
    return(x)
  }

  assert_that(length(x) <= 1)

  if (is.null(x)) {
    Verbose()
  } else if (is.logical(x)) {
    Verbose(threshold = -x)
  } else if (is.character(x)) {
    assert_that(x %in% names(verboseLevels))
    Verbose(threshold = verboseLevels[[x]] - 1)
  } else if (is.numeric(x)) {
    Verbose(threshold = x)
  } else {
    stop('unsupported verbose input')
  }
}

canShow = function(verbose, level) {
  getThreshold(verbose) < getThreshold(as.Verbose(level))
}


.enterTimed = function(verbose, ..., level = verbose$defaultLevel) {
  stopifnot(is(verbose, 'Verbose'))
  enter(verbose, ...)
  state = list(
    verbose = verbose,
    level = level,
    start = .tic()
  )
  invisible(state)
}

.exitTimed = function(state, msg = '', suffix = '...done (%s)', level = state$level, ...) {
  stopifnot(is.list(state))
  secs = .toc(state$start)
  timeText = .formatElapsedSeconds(secs)

  if (nchar(msg) == 0) {
    exit(state$verbose, ..., level = level, suffix = sprintf(suffix, timeText))
  } else {
    exit(state$verbose, sprintf(msg, timeText), level = level, suffix = '')
  }
}
