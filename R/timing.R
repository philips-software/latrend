.tic = function() {
  state = proc.time()
  invisible(state)
}

.toc = function(state) {
  secs = (proc.time() - state)['elapsed']

  if (secs < 0) {
    # can't test
    # nocov start
    warning('Elapsed time relative from .tic() state is negative! Did system time change? Returning zero.')
    secs = 0
    # nocov end
  }

  unname(secs)
}

.tocText = function(state, msg = '%s elapsed.', ...) {
  secs = .toc(state)
  timeText = .formatElapsedSeconds(secs, ...)
  sprintf(msg, timeText)
}

.formatElapsedSeconds = function(x, unit = 'auto', digits = 2) {
  assert_that(
    is.numeric(x),
    is.scalar(x),
    !is.infinite(x)
  )

  if (is.finite(x) && x < 0) {
    warning('Elapsed time is negative! Assuming zero time has elapsed.')
    x = 0
  }

  reftime = Sys.time()
  dt = difftime(reftime + x, reftime, units = unit)
  time = as.numeric(dt)
  timeUnit = attr(dt, 'units')

  if (is.null(timeUnit)) {
    # fallback scenario. Can't test.
    # nocov start
    paste(signif(x, digits), 'seconds')
    # nocov end
  } else {
    paste(signif(time, digits), timeUnit)
  }
}
