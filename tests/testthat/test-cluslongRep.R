context('latrendRep')
rngReset()

m = lcMethodKML(response = 'Value', nbRedrawing = 1, maxIt = 10)
test_that('default', {
  models = latrendRep(lcMethodKML(nbRedrawing = 1, maxIt = 10, response = 'Value'), data=testLongData, .rep = 2)

  expect_is(models, 'lcModels')
  expect_length(models, 2)

  expect_equal(deparse(getCall(models[[1]])$data), 'testLongData')
  expect_equal(deparse(getCall(models[[2]])$data), 'testLongData')
})

test_that('method var', {
  models = latrendRep(m, data = testLongData, .rep = 2)

  expect_is(models, 'lcModels')
  expect_length(models, 2)
})

test_that('single rep', {
  models = latrendRep(m, data = testLongData, .rep = 1)

  expect_is(models, 'lcModels')
  expect_length(models, 1)
})

test_that('matrix input', {
  mat = dcastRepeatedMeasures(testLongData, response = 'Value')
  models = latrendRep(m, data = mat, .rep = 2)

  expect_is(models, 'lcModels')
  expect_length(models, 2)
})

test_that('envir', {
  kml = lcMethodKML(nClusters = a, response = 'Value', nbRedrawing = 1, maxIt = 10)
  e = list2env(list(a = 1))
  models = latrendRep(kml, data = testLongData, envir = e, .rep = 2)

  expect_is(models, 'lcModels')
  expect_length(models, 2)
  expect_equal(nClusters(models[[1]]), 1)
})

test_that('repeated probabilistic method calls yield different results', {
  method = lcMethodTestRandom(alpha = 1, nClusters = 3)
  models = latrendRep(method, data = testLongData, .rep = 2)

  expect_true(!isTRUE(all.equal(trajectoryAssignments(models[[1]]), trajectoryAssignments(models[[2]]))))
})

test_that('setting .seed', {
  method = lcMethodTestRandom(alpha = 1, nClusters = 3)
  models1 = latrendRep(method, data = testLongData, .rep = 2, .seed = 1)
  models2 = latrendRep(method, data = testLongData, .rep = 2, .seed = 2)

  expect_true(!isTRUE(all.equal(trajectoryAssignments(models1[[1]]), trajectoryAssignments(models1[[2]]))))
  expect_true(!isTRUE(all.equal(trajectoryAssignments(models2[[1]]), trajectoryAssignments(models2[[2]]))))

  expect_true(!isTRUE(all.equal(trajectoryAssignments(models1[[1]]), trajectoryAssignments(models2[[1]]))))
  expect_true(!isTRUE(all.equal(trajectoryAssignments(models1[[2]]), trajectoryAssignments(models2[[2]]))))
})

test_that('method seed is ignored', {
  method = lcMethodTestRandom(alpha = 1, nClusters = 3, seed = 1)
  expect_warning({
    models = latrendRep(method, data = testLongData)
  })

  expect_true(!isTRUE(all.equal(trajectoryAssignments(models[[1]]), trajectoryAssignments(models[[2]]))))

  expect_warning({
    models2 = latrendRep(method, data = testLongData, seed = 2)
  })
  expect_true(!isTRUE(all.equal(trajectoryAssignments(models[[1]]), trajectoryAssignments(models2[[1]]))))
  expect_true(!isTRUE(all.equal(trajectoryAssignments(models2[[1]]), trajectoryAssignments(models2[[2]]))))
})
