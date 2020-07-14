context('clModels')

kml2 = m1 = cluslong(clMethodTestKML(nClusters=2), testLongData)
kml3 = cluslong(clMethodTestKML(nClusters=3), testLongData)
kml4 = cluslong(clMethodTestKML(nClusters=4), testLongData)
gmm = m2 = cluslong(clMethodTestLcmmGMM(nClusters=2), testLongData)
models = clModels(group=c(kml2, gmm), kml3, kml4)

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

test_that('as.data.frame', {
  clModels() %>%
    as.data.frame() %>%
    expect_named(c('.name', '.method', 'data')) %T>%
    {expect_equal(nrow(.), 0)}

  clModels(m1) %>%
    as.data.frame() %>%
    expect_length(length(getClMethod(m1)) + 3) %>%
    expect_named(c('.name', '.method', 'data', names(getClMethod(m1))))

  clModels(m1) %>%
    as.data.frame() %>%
    expect_length(length(getClMethod(m1)) + 3) %>%
    expect_named(c('.name', '.method', 'data', names(getClMethod(m1))))
})

test_that('subset', {
  subset(clModels(), .method == 'kml') %>%
    expect_is('clModels') %>%
    expect_length(0)

  subset(models, nClusters > 2) %>%
    expect_is('clModels') %>%
    expect_length(2)

  subset(models, nClusters > Inf) %>%
    expect_is('clModels') %>%
    expect_length(0)

  subset(models, ) %>%
    expect_is('clModels') %>%
    expect_length(4)

  subset(models, .method == 'gmm') %>%
    expect_is('clModels') %>%
    expect_length(1)

  subset(models, .method == 'gmm') %>%
    expect_length(1)

  subset(models, .method == 'kml') %>%
    expect_length(3)

  subset(models, .name == 'group1') %>%
    expect_length(1)

  subset(models, .method == 'kml' & nClusters > 2) %>%
    expect_length(2)

  subset(models, nClusters == 4, drop=TRUE) %>%
    expect_is('clModel')
})

test_that('single metric', {
  metric(clModels(), 'BIC') %>%
    expect_is('numeric') %>%
    expect_length(0)

  metric(models, 'BIC') %>%
    expect_is('numeric')

  x = metric(models, 'BIC', drop = FALSE)
  expect_is(x, 'matrix')
  expect_true(ncol(x) == 1)
  expect_true(colnames(x) == 'BIC')
})

test_that('multiple metrics', {
  metric(clModels(), c('AIC', 'BIC')) %>%
    expect_is('numeric') %>%
    expect_length(0)

  x = metric(models, c('AIC', 'BIC'))
  expect_is(x, 'matrix')
  expect_equivalent(colnames(x), c('AIC', 'BIC'))

  as.list(models) %>%
    metric(c('AIC', 'BIC')) %>%
    expect_is('matrix')
})

test_that('min', {
  min(models, 'WRSS') %>%
    expect_is('clModel')
})

test_that('max', {
  max(models, 'WRSS') %>%
    expect_is('clModel')
})

test_that('plotMetric', {
  plotMetric(models, name='BIC', subset=.method == 'kml') %>%
    expect_is('gg')

  plotMetric(models, name=c('logLik', 'BIC'), subset=.method == 'kml') %>%
    expect_is('gg')

  plotMetric(models, name=c('logLik', 'BIC'), by='nClusters', group=character()) %>%
    expect_is('gg')
})

test_that('externalMetric of list', {
  d = externalMetric(models, name = 'adjustedRand')
  expect_is(d, 'dist')
})

test_that('externalMetric against model', {
  externalMetric(models, models[[1]], name = 'adjustedRand') %>%
    expect_is('numeric') %>%
    expect_length(length(models))
})
