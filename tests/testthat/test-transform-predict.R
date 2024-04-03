context('transform-predict')

model = testModel

test_that('NULL', {
  out = transformPredict(NULL, model, newdata = data.frame(time = numeric()))
  expect_true(nrow(out) == 0)

  outClus = transformPredict(NULL, model, newdata = data.frame(Cluster = character(), time = numeric()))
  expect_true(nrow(out) == 0)
  expect_true(has_name(outClus, 'Cluster'))

  expect_error(transformPredict(NULL, model, NULL)) # newdata cannot be NULL
})

test_that('vector', {
  newdata = model.data(model)
  vec = newdata$Value

  out = transformPredict(vec, model, newdata = newdata)

  expect_is(out, 'data.frame')
  expect_true(nrow(out) == nrow(newdata))
  expect_equal(out$Fit, newdata$Value)

  out2 = transformPredict(
    1:2,
    model,
    newdata = data.frame(time = 1:2, Cluster = clusterNames(model))
  )
  expect_true(nrow(out2) == 2)

  # no cluster column
  expect_error(transformPredict(1:2, model, newdata = data.frame(time = 1:2)), 'Cluster')
  # wrong time column name
  expect_error(transformPredict(1:2, model, newdata = data.frame(Time = 1:2)), 'time')
  expect_error(transformPredict(head(vec, -1), model, newdata = newdata))
  expect_error(transformPredict(1:2, model, newdata = data.frame(time = c(1, NA))))
})

test_that('matrix', {
  newdata = model.data(model)
  assert_that(all(newdata$Cluster %in% clusterNames(model)))
  mat = matrix(rep(1:2, each = nrow(newdata)), ncol = 2)
  colnames(mat) = clusterNames(model)
  refvec = as.integer(newdata$Cluster)

  out = transformPredict(mat, model, newdata = newdata)

  expect_equal(out$Fit, refvec)

  # swap matrix
  swapmat = mat[, c(2, 1)]
  out2 = transformPredict(swapmat, model, newdata = newdata)
  expect_equal(out2, out)

  # colnames
  expect_error(transformPredict(unname(mat), model, newdata = newdata))
  # non-cluster column names
  expect_error(transformPredict(set_colnames(mat, letters[1:2]), model, newdata = newdata))
  # wrong dim
  expect_error(transformPredict(mat[, 1, drop = FALSE], model, newdata = newdata))
})

test_that('data.frame with cluster-specific prediction', {
  newdata = model.data(model)
  newdata$Fit = newdata$Value

  out = transformPredict(newdata, model, newdata = newdata)
  expect_equal(out$Fit, newdata$Value)
})

test_that('full data.frame', {
  newdata = model.data(model)
  N = nobs(model)

  df = data.frame(
    id = newdata$id,
    Fit = rep(seq_len(nClusters(model)), each = N),
    time = rep(newdata$time, nClusters(model)),
    Cluster = rep(clusterNames(model), each = N)
  )
  assert_that(nrow(df) == N * nClusters(model))

  out = transformPredict(df, model, newdata = newdata)

  # assumes cluster orders match
  expect_equal(out$Fit, as.integer(newdata$Cluster))

  expect_error(transformPredict(subset(df, select = c('Fit', 'time', 'Cluster')), model, newdata = newdata), 'id')
})
