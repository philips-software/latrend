context('partition')

refmodel = cluslong(clMethodTestKML(), testLongData)

test_that('integer assignments', {
  intAssignments = clusterAssignments(refmodel) %>% as.integer()

  model = clModelPartition(testLongData, clusterAssignments=intAssignments, nClusters=nClusters(refmodel))
  expect_valid_clModel(model)
  expect_equivalent(clusterAssignments(model), clusterAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
})

test_that('factor assignments', {
  model = clModelPartition(testLongData, clusterAssignments=clusterAssignments(refmodel), nClusters=nClusters(refmodel))
  expect_valid_clModel(model)
  expect_equivalent(clusterAssignments(model), clusterAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
})
