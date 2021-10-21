context('twostep models')
skip_if_not_installed('lme4')
rngReset()

test_that('specify', {
  repfun = function(method, data, ...) {
    as.data.table(data)[, mean(Value), by = Traj]$V1 %>% cbind()
  }
  clusfun = function(method, data, repMat, ...) {
    clusters = factor(repMat[,1] > 0, levels = c(F,T), labels = c('Low', 'High'))
    lcModelCustom(response = method$response, data = data, trajectoryAssignments = clusters)
  }
  method = lcMethodTestTwoStep(representationStep = repfun, clusterStep = clusfun, standardize = scale)

  model = expect_silent(latrend(method, testLongData))

  expect_valid_lcModel(model)
})

test_that('gckm', {
  method = lcMethodTestGCKM()
  model = expect_silent(latrend(method, testLongData))
  expect_valid_lcModel(model)
})

test_that('gckm with 1 cluster', {
  method = lcMethodTestGCKM()
  model = expect_silent(latrend(method, testLongData, nClusters = 1))
  expect_valid_lcModel(model)
})

test_that('gckm through latrendBatch', {
  method = lcMethodTestGCKM()
  models = latrendBatch(lcMethods(method, nClusters = 1:3), testLongData)
  expect_length(models, 3)
})
