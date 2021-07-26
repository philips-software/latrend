.tic = function() {
  state = proc.time()
  invisible(state)
}

.toc = function(state) {
  secs = (proc.time() - state)['elapsed']
  unname(secs)
}

.tocText = function(state, msg = '%s elapsed.', ...) {
  secs = .toc(state)
  timeText = .formatElapsedSeconds(secs, ...)
  sprintf(msg, timeText)
}

.formatElapsedSeconds = function(x, unit = 'auto', digits = 2) {
  dt = difftime(Sys.time() + x, Sys.time())
  time = as.numeric(dt)
  timeUnit = attr(dt, 'units')

  if (is.null(timeUnit)) {
    # fallback scenario
    paste(signif(secs, digits), 'seconds')
  } else {
    paste(signif(time, digits), timeUnit)
  }
}
