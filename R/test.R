#' @export
#' @title Test the implementation of an lcMethod and associated lcModel subclasses
#' @description Test a `lcMethod` subclass implementation and its resulting `lcModel` implementation.
#' @param class The name of the `lcMethod` subclass to test.
#' The class should inherit from `lcMethod`.
#' @param instantiator A `function` with signature `(id, time, response, ...)`,
#' returning an object inheriting from the `lcMethod` specified by the `class` argument.
#' @param data An optional dataset comprising three highly distinct constant clusters that will be used for testing, represented by a `data.frame`.
#' The `data.frame` must contain the columns `"Id", "Time", "Value", "Cluster"` of types `character`, `numeric`, `numeric`, and `character`, respectively.
#' All trajectories should be of equal length and have observations at the same moments in time.
#' Trajectory observations are assumed to be independent of time, i.e., all trajectories are constant.
#' This enables tests to insert additional observations as needed by sampling from the available observations.
#' @param args Other arguments passed to the instantiator function.
#' @param tests A `character` vector indicating the type of tests to run, as defined in the `*.Rraw` files inside the `/test/` folder.
#' @param maxFails The maximum number of allowed test condition failures before testing is ended prematurely.
#' @param errorOnFail Whether to throw the test errors as an error. This is always enabled while running package tests.
#' @param clusterRecovery Whether to test for correct recovery/identification of the original clusters in the test data.
#' By default, a warning is outputted.
#' @param verbose Whether the output testing results. This is always disabled while running package tests.
#' @note This is an experimental function that is subject to large changes in the future.
#' The default dataset used for testing is subject to change.
#' @examples
#' test.latrend("lcMethodRandom", clusterRecovery = "skip")
test.latrend = function(
  class = 'lcMethodKML',
  instantiator = NULL,
  data = NULL,
  args = list(),
  tests = c('method', 'basic', 'fitted', 'predict', 'cluster-single', 'cluster-three'),
  maxFails = 5L,
  errorOnFail = FALSE,
  clusterRecovery = c('warn', 'ignore', 'fail'),
  verbose = TRUE
) {
  if (is.null(instantiator)) {
    instantiator = function(id, time, response, ...) {
      new(Class = class, id = id, time = time, response = response, ...)
    }
  }

  if (is.null(data)) {
    # generate dataset
    data = generateLongData(
      sizes = rep(25L, 3L),
      data = data.frame(Time = 1:4),
      fixed = Value ~ 1,
      cluster = ~ 1,
      clusterCoefs = t(c(-1, 0, 1)),
      random = ~ 1,
      randomScales = cbind(.1, .1, .1),
      noiseScales = rep(.1, 3L),
      id = 'Id'
    ) %>%
      setnames('Class', 'Cluster')
  }

  assert_that(
    is.string(class),
    is.function(instantiator),
    is.list(args),
    is.count(maxFails),
    is.flag(errorOnFail),
    is.flag(verbose),
    is.data.frame(data),
    has_name(data, c('Id', 'Time', 'Value', 'Cluster'))
  )
  assert_that(
    has_name(formals(instantiator), '...') || has_name(formals(instantiator), c('id', 'time', 'response')),
    msg = 'instantiator argument must be a function that accepts arguments: id, time, response'
  )

  # Data validation
  data = as.data.table(data)
  data[, Id := as.character(Id)]
  data[, Cluster := as.character(Cluster)]
  assert_that(
    is.numeric(data$Time),
    is.numeric(data$Value),
    noNA(data$Id),
    noNA(data$Time),
    noNA(data$Value),
    noNA(data$Cluster),
    all(is.finite(data$Time)),
    all(is.finite(data$Value))
  )
  assert_that(uniqueN(data$Id) >= 10, msg = 'data must comprise at least 10 trajectories')
  assert_that(uniqueN(data$Cluster) == 3, msg = 'data must comprise 3 clusters')
  assert_that(
    all(data[, uniqueN(Cluster) == 1, by = Id]$V1),
    msg = 'each trajectory can only belong to 1 cluster (current data has multiple Cluster values over time per trajectory)'
  )
  assert_that(uniqueN(data$Time) >= 4, msg = 'data trajectories must have at least 4 observations')
  assert_that(
    all(data[, uniqueN(Time), by = Id]$V1 == uniqueN(data$Time)),
    msg = 'all trajectories in the data argument must have the same time'
  )
  setkeyv(data, c('Id', 'Time', 'Cluster'))

  # Determine test path
  testsDir = system.file('test-cases', package = 'latrend', mustWork = TRUE)

  if (identical(Sys.getenv('TESTTHAT'), 'true')) {
    # testthat mode
    errorOnFail = TRUE
    verbose = FALSE
  }

  if (isFALSE(verbose)) {
    # needs to happen after determining test mode, as that may change value of verbose
    cat = force
  }

  cat(sprintf('=== Testing lcMethod class "%s" ===\n', class))
  op = options(
    latrend.verbose = FALSE,
    latrend.test.checkClusterRecovery = clusterRecovery
  )
  on.exit(options(op))

  make.lcMethod = function(id = 'Traj', time = 'Moment', response = 'Y', nClusters = 1) {
    protoMethod = do.call(
      instantiator,
      c(id = id, time = time, response = response, nClusters = nClusters, args)
    )

    assert_that(
      inherits(protoMethod, class),
      msg = 'Instantiator returned object that does not inherit from the method to be tested.'
    )


    method = evaluate(protoMethod)
    method
  }

  assert_that(
    methods::isClass(class),
    msg = sprintf('class %s is not defined', class)
  )
  assert_that(
    methods::extends(class, 'lcMethod'),
    msg = sprintf('class %s does not inherit from lcMethod', class)
  )

  # gather available test files
  testFiles = list.files(path = testsDir, pattern = '\\.R$')
  assert_that(
    !identical(testFiles, 'testthat.R'),
    msg = 'test.latrend() identified the wrong test directory (testthat).\nPlease report this issue.'
  )

  assert_that(
    length(testFiles) > 0,
    msg = sprintf(
      'test.latrend() could not locate any test files.\nThis is caused by the test directory being wrongly determined.\nPath: "%s".\nPlease report this issue.',
      testsDir
    )
  )

  testNames = sub('\\.R$', '', basename(testFiles))
  assert_that(
    all(tests %in% testNames),
    msg = sprintf(
      'tests argument contains undefined tests. Valid options: %s',
      paste0('"', testNames, '"', collapse = ', ')
    )
  )

  activeTestFiles = testFiles[match(tests, testNames)]
  testFails = list()

  for (i in seq_along(tests)) {
    testContext = tests[i]
    testFilePath = file.path(testsDir, activeTestFiles[i])
    cat(sprintf('== Running tests from "%s" ==\n', testContext))

    # prepare test environment
    env = new.env()
    assign('fails', NULL, envir = env)
    assign('make.lcMethod', make.lcMethod, envir = env)
    assign('dataset', data, envir = env)

    evalEnv = new.env(parent = baseenv())
    assign('file', testFilePath, envir = evalEnv)
    assign('env', env, envir = evalEnv)

    result = evaluate::try_capture_stack(
      quote(sys.source(file, envir = env)),
      evalEnv
    )

    if (inherits(result, 'simpleError')) {
      stop(
        sprintf(
          'Unexpected error occurred while evaluating test context: "%s"\nError message:\n"%s"\nStack trace:\n%s',
          testContext,
          result$message,
          paste0(capture.output(traceback(result$calls)), collapse = '\n')
        )
      )
    }

    testFails[[testContext]] = get('fails', envir = env)
    nFails = sum(lengths(testFails))

    if (length(testFails[[testContext]]) > 0) {
      cat(sprintf('%d test(s) failed.\n', length(testFails[[testContext]])))
    } else {
      cat('Tests succeeded.\n')
    }

    if (nFails > maxFails) {
      cat(sprintf('\n\nHalting testing due to exceedance of the number of failures (%d).\n', nFails))
      break
    }
  }

  cat('\n')
  if (length(testFails) == 0) {
    cat('~~ Successfully passed all tests! ~~\n')
    TRUE
  } else {
    cat(sprintf('== %d test(s) failed for %s ==\n', sum(lengths(testFails)), class))

    # as we override cat() when verbose == FALSE, we want to call the real cat() here
    msg = capture.output({
      base::cat(sprintf('Found %d error(s) among %d test context(s)\n', sum(lengths(testFails)), length(testFails)))
      base::cat('Failed tests:\n')
      base::cat(
        sprintf(
          '  %s: %s',
          rep(names(testFails), lengths(testFails)),
          unlist(testFails)
        ),
        sep = '\n'
      )
      base::cat(sprintf('For details, see the respective test files in:\n  "%s"\n', testsDir))
    })

    if (errorOnFail) {
      stop('\n', paste0(msg, collapse = '\n'))
    } else if (verbose) {
      # only output when verbose
      cat(msg, sep = '\n')
      cat('====================\n')
    }

    FALSE
  }
}


#' @title Test a condition
#' @description Evaluate or compare an expression based on a reference expression, or test for errors.
#' The comparison of expression values is done using [base::all.equal()].
#' @param key Identifier for the test condition.
#' @param x The expression to test.
#' @param y The reference value or expression.
#' @param error Whether to expect an error, or the pattern that the error message should match.
#' @param warning Whether to expect a warning, or the pattern that the warning message should match.
#' @param runOnly Whether to skip the comparison to `y`. If disabled, an error is raised when `y` is not `TRUE`.
#' @param text Human-readable description of what this test condition is testing.
#' @param onFail How to handle a failure of the test condition. By default, this is recorded as a test failure.
#' If `onFail = "skip"`, the condition is not tested.
#' @param ... Additional arguments passed to [base::all.equal()]
#' @return Whether the test condition has passed successfully. In case of `onFail = "skip"`, `NA` is returned.
#' @details Inspired by the `data.table` package test mechanism.
#' The original motivation for this function is the lack of R support for a proper stack trace with line numbers when sourcing files,
#' which made it practically impossible to identify the offending line in a sourced file.
#' @keywords internal
#' @examples
#' \dontrun{
#' test('gt', 2 > 1)
#' test('eq', 1 + 1, 2)
#' test('lt', 2 < 1, onFail = "warn")
#' }
test = function(
    key = 'test',
    x,
    y = TRUE,
    error = FALSE,
    warning = FALSE,
    runOnly = FALSE,
    text = '',
    onFail = c('fail', 'warn', 'ignore', 'skip'),
    ...
) {
  assert_that(
    is.string(key),
    nchar(key) > 0,
    is.flag(error) || is.string(error),
    is.flag(runOnly),
    is.string(text)
  )

  # By placing "warn" as the first choice, it becomes the default option when onFail = NULL.
  # This makes the options() mechanism shorter to write, as we don't have to provide the default value
  onFail = match.arg(onFail[1], c('warn', 'fail', 'ignore', 'skip'))
  if (onFail == 'skip') {
    return (NA)
  }

  assert_that(
    !runOnly || isTRUE(y),
    msg = 'y argument cannot be altered when runOnly = TRUE, as no comparison will be performed.'
  )

  parEnv = parent.frame()
  recordFails = exists('fails', parEnv)

  if (nzchar(text)) {
    text = sprintf(' (%s)', text)
  }

  success = function() {
    TRUE
  }

  fail = function(msg) {
    switch(onFail,
      fail = {
        cat(msg)
        if (recordFails) {
          assign(
            'fails',
            append(get('fails', parEnv), key),
            parEnv
          )
        }
      },
      warn = {
        warning(msg)
      }
    )

    FALSE
  }

  xError = NULL
  xWarn = NULL

  xEval = withCallingHandlers(
    withRestarts(force(x), muffle = function() {}),
    error = function(cond) {
      xError <<- conditionMessage(cond)
      invokeRestart('muffle')
    },
    warning = function(cond) {
      xWarn <<- conditionMessage(cond)
      tryInvokeRestart('muffleWarning')
    },
    message = function(cond) {
      tryInvokeRestart('muffleMessage')
    }
  )

  yEval = suppressMessages(suppressWarnings(force(y)))

  if (length(xWarn) > 0) {
    # warning occurred
    if (!isFALSE(warning)) {
      # expected warning
      if (!is.flag(warning)) {
        # check if error message matches pattern
        if (grepl(warning, xWarn)) {
          # OK, proceed
        } else {
          # FAIL: warning message mismatch
          msg = sprintf('Test "%s"%s generated warning "%s", but "%s" was expected.\n', key, text, xWarn, warning)
          return(fail(msg))
        }
      } else {
        # OK, proceed
      }
    } else {
      # FAIL: unexpected warning
      msg = sprintf('Test "%s"%s generated unexpected warning "%s".\n', key, text, xWarn)
      return(fail(msg))
    }
  } else if (!isFALSE(warning)) {
    # no warning, but was expected
    msg = sprintf('Test "%s"%s did not generate a warning.\n', key, text)
    return(fail(msg))
  }

  if (length(xError) > 0) {
    # error occurred
    if (!isFALSE(error)) {
      # expected error
      if (!is.flag(error)) {
        # check if error message matches pattern
        if (grepl(error, xError)) {
          # OK
          success()
        } else {
          # FAIL: error message mismatch
          msg = sprintf('Test "%s"%s generated error "%s", but "%s" was expected.\n', key, text, xError, error)
          fail(msg)
        }
      } else {
        # OK
        success()
      }
    } else {
      # FAIL: Unexpected error occurred
      msg = sprintf('Test "%s"%s generated unexpected error "%s".\n', key, text, xError)
      fail(msg)
    }
  }
  else if (!isFALSE(error)) {
    # no error, but was expected
    msg = sprintf('Test "%s"%s did not generate an error.\n', key, text)
    fail(msg)
  } else if (runOnly) {
    # no comparison
    success()
  } else {
    # no error. compare values
    comparison = all.equal(x, y, ...)
    if (isTRUE(comparison)) {
      # OK
      success()
    } else {
      # FAIL: x != y
      msg = sprintf('Test "%s"%s did not generate the expected result:\n%s\n', key, text, paste0('  ', comparison, collapse = '\n'))
      fail(msg)
    }
  }
}
