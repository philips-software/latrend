context('twostep model')

test_that('specify', {
  repfun = function(method, data) {
    data[, mean(Value), by=Id]$V1 %>% cbind
  }
  clusfun = function(method, data, repMat, ...) {
    clusters = factor(repMat[,1] > 0, levels=c(F,T), labels=c('Low', 'High'))
    clModelCustom(data=data, clusterAssignments=clusters)
  }
  method = clMethodTwoStep(representationStep=repfun, clusterStep=clusfun, standardize=scale)

  model = cluslong(method, testLongData)

  expect_valid_clModel(model)
})