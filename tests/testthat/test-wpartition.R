context('weighted partition')

refmodel = cluslong(clMethodTestKML(), testLongData)

test_that('default', {
  model = clModelWeightedPartition(testLongData, weights=postprob(refmodel))

  expect_valid_clModel(model)
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(clusterAssignments(model), clusterAssignments(refmodel))
  expect_equivalent(postprob(model), postprob(refmodel))
})

test_that('non-unit weights', {
  model = clModelWeightedPartition(testLongData, weights=2 * postprob(refmodel))

  expect_valid_clModel(model)
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(clusterAssignments(model), clusterAssignments(refmodel))
  expect_equivalent(postprob(model), postprob(refmodel))
})
