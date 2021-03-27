skip_on_cran()
skip_on_ci()
skip_if(parallel::detectCores(logical = FALSE) < 2)

skip_if_not_installed('parallel')
cl = parallel::makeCluster(2)

skip_if_not_installed('doParallel')
doParallel::registerDoParallel(cl)


setClass('lcMethodSleep', contains = 'lcMethodRandom')
setMethod('fit', signature('lcMethodSleep'), function(method, data, envir, verbose, ...) {
  Sys.sleep(method$sleep)

  callNextMethod()
})

mSleep = lcMethod('lcMethodSleep',
  response = 'Value',
  alpha = 10,
  sleep = 1,
  center = meanNA,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  nClusters = 2,
  name = 'random')

# need a long sleep time to counteract the large start-up overhead time in Windows
mSleep10 = update(mSleep, sleep = 10)

test_that('non-parallel latrendRep', {
  time = system.time({
    latrendRep(mSleep, data = testLongData, .rep = 2, .parallel = FALSE)
  })
  expect_gt(time['elapsed'], 1.5)
})

test_that('parallel latrendRep', {
  time = system.time({
    latrendRep(mSleep10, data = testLongData, .rep = 2, .parallel = TRUE)
  })
  expect_lt(time['elapsed'], 18)
})

parallel::stopCluster(cl)
