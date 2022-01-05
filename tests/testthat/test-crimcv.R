context('crimcv')
# skip on CI because crimCV fitting fails at random with error "object 'Frtr' not found"
skip_on_ci()
skip_on_cran()
skip_if_not_installed('crimCV')
rngReset()

library(crimCV)
data(TO1adj)
subTO1adj = TO1adj[1:100, seq(1, ncol(TO1adj), by = 2)]

lcMethodTestCrimCV = function(...) {
  lcMethodCrimCV(
    response = 'Value',
    ...,
    model = 'ZIP',
    dpolyp = 2,
    dpolyl = 1,
    init = 5,
    seed = 1
  )
}

lcMethodTestCrimCVt = function(...) {
  lcMethodCrimCV(
    response = 'Value',
    ...,
    model = 'ZIPt',
    dpolyp = 2,
    init = 5,
    seed = 1
  )
}

test_that('default tau', {
  suppressWarnings({
    model = latrend(lcMethodTestCrimCVt(), subTO1adj)
    expect_valid_lcModel(model)
  })

  p = plot(model, what = 'nu')
  expect_is(p, 'ggplot')
})

test_that('default', {
  suppressWarnings({
    model = latrend(lcMethodTestCrimCV(), subTO1adj)
    expect_valid_lcModel(model)
  })

  p = plot(model, what = 'nu')
  expect_is(p, 'ggplot')
})

test_that('many clusters', {
  skip_on_cran()
  suppressWarnings({
    model = latrend(lcMethodTestCrimCVt(nClusters = 3), subTO1adj)
    expect_valid_lcModel(model)
  })
})
