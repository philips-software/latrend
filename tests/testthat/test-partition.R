context('partition')
rngReset()

refmodel = latrend(lcMethodTestKML(), testLongData)

test_that('integer assignments', {
  intAssignments = clusterAssignments(refmodel) %>% as.integer()

  model = lcModelPartition(testLongData,
                           response = 'Value',
                           clusterAssignments = intAssignments,
                           nClusters = nClusters(refmodel))
  expect_valid_lcModel(model)
  expect_equivalent(clusterAssignments(model), clusterAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
})

test_that('factor assignments', {
  model = lcModelPartition(testLongData,
                           response = 'Value',
                           clusterAssignments = clusterAssignments(refmodel))
  expect_valid_lcModel(model)
  expect_equivalent(clusterAssignments(model), clusterAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
})

test_that('local data', {
  {
    a = testLongData
    model = lcModelPartition(a,
                             response = 'Value',
                             clusterAssignments = clusterAssignments(refmodel))
  }

  expect_valid_lcModel(model)
})
