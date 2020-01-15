context('cluslong')

test_that('cluslong', {
  cluslong(clMethodKML(), data=testLongData) %>%
    expect_is('clModel')
})

test_that('cluslong with overwritten argument', {
  cluslong(clMethodKML(), data=testLongData, nClusters=1) %T>%
    {expect_equal(nClusters(.), 1)}
})

test_that('cluslong with new arguments', {
  cluslong(clMethodKML(), data=testLongData, test=2) %T>%
    {expect_equal(getMethod(.)$test, 2)}
})

test_that('cluslongRep', {
  cluslongRep(clMethodKML(), data=testLongData, .rep=2) %>%
    expect_is('clModels') %>%
    expect_length(2)
})