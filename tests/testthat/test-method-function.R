rngReset()

test_that('default', {
  clusfun = function(data, ...) {
    clusters = as.data.table(data) %>%
      {.[, mean(Value) > 0, by = Traj]$V1} %>%
      factor(levels = c(F,T), labels = c('Low', 'High'))

    lcModelPartition(data = data, response = 'Value', trajectoryAssignments = clusters)
  }
  method = lcMethodFunction(response = 'Value', fun = clusfun)

  model = latrend(method, testLongData)

  expect_valid_lcModel(model)
})
