context('random')

test_that('default', {
  model = cluslong(clMethodRandom(alpha=1, nClusters=3), data=testLongData, seed=1) %T>%
    expect_valid_clModel()

  expect_equal(nClusters(model), 3)
  expect_equivalent(clusterProportions(model), c(.05, .49, .46))
})

test_that('uniform groups', {
  model = cluslong(clMethodRandom(alpha=1e3, nClusters=8), data=testLongData, seed=1) %T>%
    expect_valid_clModel()

  expect_equal(nClusters(model), 8)
  expect_true(all(clusterProportions(model) > .1))
})

test_that('single group', {
  model = cluslong(clMethodRandom(alpha=1, nClusters=1), data=testLongData, seed=1) %T>%
    expect_valid_clModel()

  expect_equal(nClusters(model), 1)
  expect_equivalent(clusterProportions(model), 1)
})