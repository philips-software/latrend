context('transform-fitted')

model = testModel

test_that('NULL', {
  out = transformFitted(NULL, model)
  expect_true(all(is.na(out)))
})

test_that('matrix', {
  refdata = model.data(model)
  mat = matrix(refdata$Value, nrow = nobs(model), ncol = nClusters(model))
  colnames(mat) = clusterNames(model)

  out = transformFitted(mat, model, clusters = NULL)

  expect_is(out, 'matrix')
  expect_true(noNA(out))
  expect_true(nrow(out) == nobs(model))
  expect_true(ncol(out) == nClusters(model))
})

test_that('matrix in swapped column order', {
  refmat = matrix(rep(1:2, each = nobs(model)), ncol = 2)
  colnames(refmat) = clusterNames(model)

  # reverse column order
  mat = refmat[ , c(2, 1)]

  out = transformFitted(mat, model, clusters = NULL)

  expect_equal(out, refmat)
})

test_that('matrix cluster', {
  mat = matrix(rep(1:2, each = nobs(model)), ncol = 2)
  colnames(mat) = clusterNames(model)

  out = transformFitted(mat, model, clusters = trajectoryAssignments(model))

  expect_true(is.numeric(out))
  expect_length(out, nobs(model))
  expect_equal(out, as.integer(trajectoryAssignments(model)[make.idRowIndices(model)]))
})

test_that('list', {
  mat = matrix(rep(1:2, each = nobs(model)), ncol = 2)
  colnames(mat) = clusterNames(model)

  # specify in reverse order to check if names are used
  lis = list(
    data.frame(Fit = rep(2, nobs(model))),
    data.frame(Fit = rep(1, nobs(model)))
  ) %>%
    set_names(rev(clusterNames(model)))

  out = transformFitted(lis, model, clusters = NULL)
  expect_equal(out, mat)
})

test_that('data.frame', {
  df = data.frame(
    Fit = rep(1:2, each = nobs(model)),
    Cluster = rep(clusterNames(model), each = nobs(model))
  )

  out = transformFitted(df, model, clusters = NULL)
  expect_is(out, 'matrix')
  expect_true(all(out[, 1] == 1))
  expect_true(all(out[, 2] == 2))
})

test_that('reversed data.frame', {
  df = data.frame(
    Fit = rep(2:1, each = nobs(model)),
    Cluster = rep(rev(clusterNames(model)), each = nobs(model))
  )

  out = transformFitted(df, model, clusters = NULL)
  expect_equal(colnames(out), clusterNames(model))
  expect_true(all(out[, 1] == 1))
  expect_true(all(out[, 2] == 2))
})
