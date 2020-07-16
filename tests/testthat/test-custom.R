context('custom model')

test_that('default', {
  clusfun = function(data, ...) {
       clusters = data[, mean(Value) > 0, by=Id]$V1 %>%
           factor(levels=c(F,T), labels=c('Low', 'High'))
       list(clusters=clusters)
       lcModelCustom(data=data, clusterAssignments=clusters)
  }
  method = lcMethodCustom(fun=clusfun)

  model = latrend(method, testLongData)

  expect_valid_lcModel(model)
})
