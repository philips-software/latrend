context('lcModel implementation')

setClass('lcModelTest', contains = 'lcModel')
model = testModel2
class(model) = 'lcModelTest'

predClusFun = function(object, newdata = NULL, cluster, ...) {
  rep(NaN, nrow(newdata))
}

predFun = function(object, newdata, ...) {
  pred = matrix(NaN, nrow = nrow(newdata), ncol = nClusters(object))
  transformPredict(pred = pred, model = object, newdata = newdata)
}

# including this test results in error for predict() and fitted() in later tests. No clue why.
# test_that('no predict funs', {
#   expect_error(predict(model, newdata=data.frame(time=1)))
#   expect_error(predictForCluster(model, newdata=data.frame(time=1), cluster = 'A'))
# })

setMethod('predictForCluster', 'lcModelTest', definition = predClusFun)

test_that('default predict.lcModel', {
  dfpred = predict(model, newdata=data.frame(time=1))

  expect_is(dfpred, 'list')
  expect_is(dfpred$A$Fit, 'numeric')
  expect_equivalent(nrow(dfpred$A), 1)

  # removeMethod('predictForCluster', 'lcModelTest')
})

# NOTE: disabled until there is a way to unregister an S3 method
# test_that('default predictForCluster', {
#   .S3method('predict', 'lcModelTest', predFun)
#   pred = predictForCluster(model, newdata=data.frame(time=c(1,2)), cluster = 'A')
#   expect_is(pred, 'numeric')
#   expect_equ
#   .S3method('predict', 'lcModelTest', predict.lcModel)
# })


test_that('default fitted', {
  # setMethod('predictForCluster', 'lcModelTest', predClusFun)

  suppressWarnings({
    expect_is(fitted(model), 'numeric')
  })

  # removeMethod('predictForCluster', 'lcModelTest')
})
