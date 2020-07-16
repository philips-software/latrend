context('partition')

refmodel = latrend(lcMethodTestKML(), testLongData)

test_that('integer assignments', {
  intAssignments = clusterAssignments(refmodel) %>% as.integer()

  model = lcModelPartition(testLongData, clusterAssignments=intAssignments, nClusters=nClusters(refmodel))
  expect_valid_lcModel(model)
  expect_equivalent(clusterAssignments(model), clusterAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
})

test_that('factor assignments', {
  model = lcModelPartition(testLongData, clusterAssignments=clusterAssignments(refmodel))
  expect_valid_lcModel(model)
  expect_equivalent(clusterAssignments(model), clusterAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
})
