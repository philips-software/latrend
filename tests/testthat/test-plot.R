context('plot')
rngReset()

test_that('plotTrajectories.data.frame', {
  p = plotTrajectories(testLongData, response = 'Value')
  expect_true(is.ggplot(p))
})

test_that('plotClusterTrajectories.data.frame', {
  p = plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Class')
  expect_true(is.ggplot(p))
})

test_that('plotClusterTrajectories.data.frame with trajectories', {
  p = plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Class', trajectories = TRUE)
  expect_true(is.ggplot(p))
})
