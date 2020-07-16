context('twostep models')

test_that('specify', {
  repfun = function(method, data, ...) {
    data[, mean(Value), by=Id]$V1 %>% cbind
  }
  clusfun = function(method, data, repMat, ...) {
    clusters = factor(repMat[,1] > 0, levels=c(F,T), labels=c('Low', 'High'))
    lcModelCustom(response = method$response, data=data, clusterAssignments=clusters)
  }
  method = lcMethodTestTwoStep(representationStep=repfun, clusterStep=clusfun, standardize=scale)

  model = expect_silent(latrend(method, testLongData))

  expect_valid_lcModel(model)
})

test_that('gckm', {
  method = lcMethodTestGCKM()
  model = expect_silent(latrend(method, testLongData))
  expect_valid_lcModel(model)
})
