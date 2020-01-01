context('custom model')

test_that('default', {
  clusfun = function(data, ...) {
       clusters = data[, mean(Value) > 0, by=Id]$V1 %>%
           factor(levels=c(F,T), labels=c('Low', 'High'))
       list(clusters=clusters)
       clModelCustom(data=data, clusterAssignments=clusters)
  }
  method = clMethodCustom(fun=clusfun)

  model = cluslong(method, testLongData)

  expect_valid_clModel(model)
})