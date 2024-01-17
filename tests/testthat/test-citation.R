test_that('nothing to cite', {
  .clearCited()
  expect_silent({
    .suggestCiteMethod(testModel1)
  })

  expect_silent({
    .suggestCiteMethod(testModel3)
  })
})


test_that('method citation', {
  skip_if_not_installed('kml')
  method = lcMethodKML('Y', id = 'Id', time = 'Time', nClusters = 3)
  .clearCited()

  options(latrend.cited.lcMethodKML = FALSE)
  expect_message({
    .suggestCiteMethod(method)
  }, 'lcMethodKML')

  # should not print on the second run
  expect_silent({
    .suggestCiteMethod(method)
  })

  # should not print when option is set
  .clearCited()
  options(latrend.cited.lcMethodKML = TRUE)
  expect_silent({
    .suggestCiteMethod(method)
  })
})
