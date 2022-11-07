test_that('specify default', {
  metaMethod = lcFitRep(mRandom, metric = 'RSS', maximize = FALSE)
  expect_s4_class(metaMethod, 'lcFitRep')
  expect_true(has_lcMethod_args(metaMethod, 'rep'))
})

test_that('specify min', {
  metaMethod = lcFitRepMin(mRandom, metric = 'RSS')
  expect_false(metaMethod$maximize)
})

test_that('specify max', {
  metaMethod = lcFitRepMax(mRandom, metric = 'ASW')
  expect_true(metaMethod$maximize)
})

test_that('fit', {
  metaMethod = lcFitRepMin(mRandom, metric = 'RSS')

  out = capture.output({
    model = latrend(metaMethod, testLongData, verbose = verboseLevels$finest)
  }, type = 'message')

  expect_match(paste0(out, collapse = '\n'), regexp = 'RSS')
})
