context('twostep models')

test_that('specify', {
  repfun = function(method, data, ...) {
    data[, mean(Value), by=Id]$V1 %>% cbind
  }
  clusfun = function(method, data, repMat, ...) {
    clusters = factor(repMat[,1] > 0, levels=c(F,T), labels=c('Low', 'High'))
    clModelCustom(data=data, clusterAssignments=clusters)
  }
  method = clMethodTwoStep(representationStep=repfun, clusterStep=clusfun, standardize=scale)

  model = expect_silent(cluslong(method, testLongData))

  expect_valid_clModel(model)
})

test_that('gckm', {
  method = clMethodGCKM()
  model = expect_silent(cluslong(method, testLongData))
  expect_valid_clModel(model)
})