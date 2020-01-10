context('clModels')

m1 = cluslong(clMethodKML(nRuns=1), testLongData)
m2 = cluslong(clMethodGMM(), testLongData)

test_that('as', {
  as.clModels(NULL) %>%
    expect_is('clModels') %>%
    expect_length(0)

  as.clModels(m1) %>%
    expect_is('clModels') %>%
    expect_length(1)

  as.clModels(c(m1, m2)) %>%
    expect_is('clModels') %>%
    expect_length(2)

  as.clModels(c(A=m1, B=m2)) %>%
    expect_length(2) %>%
    expect_named()
})

test_that('create', {
  clModels() %>%
    expect_is('clModels') %>%
    expect_length(0)

  clModels(m1) %>%
    expect_is('clModels') %>%
    expect_length(1)

  clModels(a=m1) %>%
    expect_is('clModels') %>%
    expect_length(1) %>%
    expect_named('a')

  clModels(a=m1, b=m2) %>%
    expect_is('clModels') %>%
    expect_length(2) %>%
    expect_named(c('a', 'b'))

  clModels(c(a=m1, b=m2)) %>%
    expect_is('clModels') %>%
    expect_length(2) %>%
    expect_named(c('a', 'b'))

  clModels(a=c(a=m1, b=m2)) %>%
    expect_is('clModels') %>%
    expect_length(2) %>%
    expect_named(c('a.a', 'a.b'))

  clModels(a=c(a=m1, b=m2), b=m1) %>%
    expect_is('clModels') %>%
    expect_length(3) %>%
    expect_named(c('a.a', 'a.b', 'b'))
})

test_that('subset', {
  kml1 = cluslong(clMethodKML(nClusters=1, nRuns=1), testLongData)
  kml2 = cluslong(clMethodKML(nClusters=2, nRuns=1), testLongData)
  kml3 = cluslong(clMethodKML(nClusters=3, nRuns=1), testLongData)
  gmm = cluslong(clMethodGMM(), testLongData)

  models = clModels(group=c(kml1, gmm), kml2, kml3)
  subset(models, nClusters > 1) %>%
    expect_is('clModels') %>%
    expect_length(3)

  subset(models, nClusters > Inf) %>%
    expect_is('clModels') %>%
    expect_length(0)

  subset(models, ) %>%
    expect_is('clModels') %>%
    expect_length(4)

  subset(models, method == 'gmm') %>%
    expect_is('clModels') %>%
    expect_length(1)

  subset(models, method == 'gmm') %>%
    expect_length(1)

  subset(models, method == 'kml') %>%
    expect_length(3)

  subset(models, method == 'kml' & nClusters > 1) %>%
    expect_length(2)
})