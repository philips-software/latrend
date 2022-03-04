#' @export
#' @title Test the implementation of an lcMethod and associated lcModel subclasses
#' @description Test a `lcMethod` subclass implementation and its resulting `lcModel` implementation.
#' @param class The name of the `lcMethod` subclass to test.
#' The class should inherit from `lcMethod`.
#' @param instantiator A `function` with signature `(id, time, response, ...)`,
#' returning an object inheriting from the `lcMethod` specified by the `class` argument.
#' @param args Other arguments passed to the instantiator function.
#' @param tests A `character` vector indicating the type of tests to run, as defined in the `*.Rraw` files inside the `/test/` folder.
#' @param errorOnFail Whether to throw the test errors as an error. This is always enabled while running package tests.
#' @param verbose Whether the output testing results. This is always disabled while running package tests.
#' @note This is an experimental function that is subject to large changes in the future.
#' @examples
#' test.latrend("lcMethodRandom")
test.latrend = function(
  class = 'lcMethodKML',
  instantiator = NULL,
  args = list(),
  tests = c('method', 'basic', 'fitted', 'predict', 'cluster-single', 'cluster-three'),
  maxFails = 5L,
  errorOnFail = FALSE,
  verbose = TRUE
) {
  if (is.null(instantiator)) {
    instantiator = function(id, time, response, ...) {
      new(Class = class, id = id, time = time, response = response, ...)
    }
  }

  assert_that(
    is.string(class),
    is.function(instantiator),
    is.list(args),
    is.count(maxFails),
    is.flag(errorOnFail),
    is.flag(verbose)
  )

  assert_that(
    has_name(formals(instantiator), '...') || has_name(formals(instantiator), c('id', 'time', 'response')),
    msg = 'instantiator argument must be a function that accepts arguments: id, time, response'
  )

  if (identical(Sys.getenv('TESTTHAT'), 'true')) {
    # testthat mode
    errorOnFail = TRUE
    verbose = FALSE
    pkgDir = dirname(dirname(getwd()))
    testsDir = file.path(pkgDir, 'inst', 'tests')
  } else if (exists('test.latrend', .GlobalEnv, inherits = FALSE) ||
             identical(devtools::dev_packages(), 'latrend')) {
    # dev mode
    pkgDir = getwd()
    testsDir = file.path(pkgDir, 'inst', 'tests')
  } else {
    # install mode
    pkgDir = getNamespaceInfo('latrend', 'path')
    testsDir = file.path(pkgDir, 'tests')
  }

  if (isFALSE(verbose)) {
    # needs to happen after determining test mode, as that may change value of verbose
    cat = force
  }

  cat(sprintf('=== Testing lcMethod class "%s" ===\n', class))
  op = options(
    latrend.verbose = FALSE
  )
  on.exit(options(op))

  make.lcMethod = function(id = 'Traj', time = 'Moment', response = 'Y', nClusters = 1) {
    m = do.call(
      instantiator,
      c(id = id, time = time, response = response, nClusters = nClusters, args)
    )

    assert_that(
      inherits(m, class),
      msg = 'Instantiator returned object that does not inherit from the method to be tested.'
    )

    evaluate(m)
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
    test = tests[i]
    testFilePath = file.path(testsDir, activeTestFiles[i])
    cat(sprintf('== Running tests from "%s" ==\n', test))

    # prepare test environment
    env = new.env(parent = sys.frame())
    assign('fails', NULL, envir = env)
    assign('make.lcMethod', make.lcMethod, envir = env)

    sys.source(testFilePath, envir = env)

    testFails[[test]] = get('fails', envir = env)
    nFails = sum(lengths(testFails))

    if (length(testFails[[test]]) > 0) {
      cat(sprintf('%d test(s) failed.\n', length(testFails[[test]])))
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
#' @param ... Additional arguments passed to [base::all.equal()]
#' @return Whether the test passes.
#' @details Inspired by the `data.table` package test mechanism.
#' The original motivation for this function is the lack of R support for a proper stack trace with line numbers when sourcing files,
#' which made it practically impossible to identify the offending line in a sourced file.
#' @keywords internal
#' @examples
#' test('gt', 2 > 1)
#' test('eq', 1 + 1, 2)
test = function(key = 'test', x, y = TRUE, error = FALSE, warning = FALSE, runOnly = FALSE, text = '', ...) {
  assert_that(
    is.string(key),
    nchar(key) > 0,
    is.flag(error) || is.string(error),
    is.flag(runOnly),
    is.string(text)
  )

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

  fail = function() {
    if (recordFails) {
      assign(
        'fails',
        append(get('fails', parEnv), key),
        parEnv
      )
    }
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
          cat(sprintf('Test "%s"%s generated warning "%s", but "%s" was expected.\n', key, text, xWarn, warning))
          return(fail())
        }
      } else {
        # OK, proceed
      }
    } else {
      # FAIL: unexpected warning
      cat(sprintf('Test "%s"%s generated unexpected warning "%s".\n', key, text, xWarn))
      return(fail())
    }
  } else if (!isFALSE(warning)) {
    # no warning, but was expected
    cat(sprintf('Test "%s"%s did not generate a warning.\n', key, text))
    return(fail())
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
          cat(sprintf('Test "%s"%s generated error "%s", but "%s" was expected.\n', key, text, xError, error))
          fail()
        }
      } else {
        # OK
        success()
      }
    } else {
      # FAIL: Unexpected error occurred
      cat(sprintf('Test "%s"%s generated unexpected error "%s".\n', key, text, xError))
      fail()
    }
  }
  else if (!isFALSE(error)) {
    # no error, but was expected
    cat(sprintf('Test "%s"%s did not generate an error.\n', key, text))
    fail()
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
      cat(sprintf('Test "%s"%s did not generate the expected result:\n%s\n', key, text, paste0('  ', comparison, collapse = '\n')))
      fail()
    }
  }
}
