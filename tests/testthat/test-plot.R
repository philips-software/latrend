context('plot')

test_that('plotTrajectories.data.frame', {
  p = plotTrajectories(testLongData, response = 'Value')
  expect_true(is.ggplot(p))
})

test_that('plotTrajectories.data.frame with cluster column', {
  p = plotTrajectories(testLongData, response = 'Value', cluster = 'Cluster')
  expect_true(is.ggplot(p))
})

test_that('plotTrajectories.data.frame with cluster vector', {
  clusters = testLongData[, first(Cluster), keyby=Id]$V1
  p = plotTrajectories(testLongData, response = 'Value', cluster = clusters)
  expect_true(is.ggplot(p))
})

test_that('plotClusterTrajectories.data.frame', {
  p = plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Cluster')
  expect_true(is.ggplot(p))
})

test_that('plotClusterTrajectories.data.frame with showTrajs', {
  p = plotClusterTrajectories(testLongData, response = 'Value', cluster = 'Cluster', showTrajs = TRUE)
  expect_true(is.ggplot(p))
})
