context('clMethods')

test_that('default', {
  clMethods(clMethodTestKML()) %>%
    expect_is('list') %>%
    expect_length(1)
})

test_that('var', {
  kml = clMethodTestKML()
  clMethods(kml) %>%
    expect_is('list') %>%
    expect_length(1)
})


test_that('scalar argument', {
  clMethods(clMethodTestKML(), nClusters=2) %>%
    expect_is('list') %>%
    expect_length(1) %T>%
    {expect_equal(.[[1]]$nClusters, 2)}
})

test_that('vector argument', {
  clMethods(clMethodTestKML(), testLongData, nClusters=1:3) %>%
    expect_is('list') %>%
    expect_length(3)
})

test_that('var with vector argument', {
  kml = clMethodTestKML()
  clMethods(kml, testLongData, nClusters=1:3) %>%
    expect_is('list') %>%
    expect_length(3)
})

test_that('.() argument', {
  a = 1
  b = 2
  methods = clMethods(clMethodTestKML(), testLongData, nClusters=.(a,b)) %>%
    expect_is('list') %>%
    expect_length(2)

  expect_equal(deparse(methods[[1]][['nClusters', eval=FALSE]]), 'a')
  expect_equal(deparse(methods[[2]][['nClusters', eval=FALSE]]), 'b')
})

