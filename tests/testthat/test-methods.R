context('lcMethods')

test_that('default', {
  lcMethods(lcMethodTestKML()) %>%
    expect_is('list') %>%
    expect_length(1)
})

test_that('var', {
  kml = lcMethodTestKML()
  lcMethods(kml) %>%
    expect_is('list') %>%
    expect_length(1)
})


test_that('scalar argument', {
  lcMethods(lcMethodTestKML(), nClusters=2) %>%
    expect_is('list') %>%
    expect_length(1) %T>%
    {expect_equal(.[[1]]$nClusters, 2)}
})

test_that('vector argument', {
  lcMethods(lcMethodTestKML(), testLongData, nClusters=1:3) %>%
    expect_is('list') %>%
    expect_length(3)
})

test_that('var with vector argument', {
  kml = lcMethodTestKML()
  lcMethods(kml, testLongData, nClusters=1:3) %>%
    expect_is('list') %>%
    expect_length(3)
})

test_that('.() argument', {
  a = 1
  b = 2
  methods = lcMethods(lcMethodTestKML(), testLongData, nClusters=.(a,b)) %>%
    expect_is('list') %>%
    expect_length(2)

  expect_equal(deparse(methods[[1]][['nClusters', eval=FALSE]]), 'a')
  expect_equal(deparse(methods[[2]][['nClusters', eval=FALSE]]), 'b')
})

