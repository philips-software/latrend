#' @export
#' @title Test a lcMethod implementation
#' @description Test a `lcMethod` subclass implementation including the outputted `lcModel` class.
#' @param class The name of the `class` inheriting from `lcMethod` to test.
#' @param instantiator A `function` with signature `(id, time, response)`
#' @param args Other arguments passed to the instantiator function.
#' returning an object inheriting from the `lcMethod` specified by the `class` argument.
#' @note This is an experimental function that is subject to large changes in the future.
#' @examples
#' testLcMethod("lcMethodRandom")
testLcMethod = function(
  class,
  instantiator = NULL,
  args = list()
) {
  loadNamespace('testthat')

  if (is.null(instantiator)) {
    instantiator = function(id, time, response, ...) {
      new(Class = class, id = id, time = time, response = response, ...)
    }
  }

  assert_that(
    is.character(class),
    is.scalar(class),
    is.function(instantiator),
    has_name(formals(instantiator), c('id', 'time', 'response')),
    is.list(args)
  )

  makeMethod = function(id = 'Traj', time = 'Moment', response = 'Y', nClusters = 1) {
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

  expect_true(methods::isClass(class), info = sprintf('class %s is not defined', class))
  expect_true(
    methods::extends(class, 'lcMethod'),
    info = sprintf('class %s does not inherit from lcMethod', class)
  )

  # Basic method functionality
  .testLcMethodCore(class, makeMethod)

  # Single-cluster fitting
  .testLcMethodSingleCluster(class, makeMethod)
}

.testLcMethodCore = function(class, makeMethod) {
  mTest = makeMethod(id = 'Test.Id', time = 'Test.Time', response = 'Test.Response')

  expect_true(nchar(getName(mTest)) > 0)
  expect_true(nchar(getShortName(mTest)) > 0)
  expect_type(getLabel(mTest), 'character')
  expect_equal(idVariable(mTest), 'Test.Id')
  expect_equal(timeVariable(mTest), 'Test.Time')
  expect_equal(responseVariable(mTest), 'Test.Response')
  getArgumentDefaults(mTest) # testing is done by the generic
  getArgumentExclusions(mTest) # testing is done by the generic
}

.testLcMethodSingleCluster = function(class, makeMethod) {
  m1 = makeMethod(id = 'Traj', time = 'Moment', response = 'Y')
  testData1 = data.table(
    Traj = rep(paste0('S', 1:50), each = 5),
    Moment = rep(1:5, 50),
    Y = rnorm(250)
  )

  mod1 = latrend(m1, testData1)
  expect_equal(nClusters(mod1), 1)
  expect_equal(nIds(mod1), 50)
  expect_equal(nobs(mod1), nrow(testData1))
  expect_setequal(ids(mod1), unique(testData1$Traj))
  expect_setequal(time(mod1), unique(testData1$Moment))
  expect_equivalent(clusterProportions(mod1), 1)
  expect_equivalent(clusterSizes(mod1), 50)
  expect_equivalent(postprob(mod1)[, 1], rep(1, 50))
}
