context('partition')
rngReset()

refmodel = modelTest

test_that('integer assignments', {
  intAssignments = trajectoryAssignments(refmodel) %>% as.integer()

  model = lcModelPartition(
    testLongData,
    response = 'Value',
    trajectoryAssignments = intAssignments,
    nClusters = nClusters(refmodel)
  )

  expect_valid_lcModel(model)
  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
})


test_that('factor assignments', {
  model = lcModelPartition(
    testLongData,
    response = 'Value',
    trajectoryAssignments = trajectoryAssignments(refmodel)
  )

  expect_valid_lcModel(model)
  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(clusterNames(model), clusterNames(refmodel))
})


test_that('table assignments', {
  model = lcModelPartition(
    testLongData,
    response = 'Value',
    trajectoryAssignments = data.frame(
      Traj = ids(refmodel),
      Cluster = trajectoryAssignments(refmodel))
  )

  expect_valid_lcModel(model)
  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(clusterNames(model), clusterNames(refmodel))
})


test_that('data column assignment', {
  model = lcModelPartition(
    testLongData,
    response = 'Value',
    trajectoryAssignments = 'Class'
  )

  expect_valid_lcModel(model)
  expect_true(externalMetric(model, refmodel, 'adjustedRand') >= .99)
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(clusterNames(model), clusterNames(refmodel))
})


test_that('character assignments', {
  model = lcModelPartition(
    testLongData,
    response = 'Value',
    trajectoryAssignments = as.character(trajectoryAssignments(refmodel))
  )

  expect_valid_lcModel(model)
  expect_equivalent(trajectoryAssignments(model), trajectoryAssignments(refmodel))
  expect_equivalent(nClusters(model), nClusters(refmodel))
  expect_equivalent(clusterNames(model), clusterNames(refmodel))
})


test_that('local data', {
  {
    a = testLongData
    model = lcModelPartition(
      a,
      response = 'Value',
      trajectoryAssignments = trajectoryAssignments(refmodel)
    )
  }

  expect_valid_lcModel(model)
})
