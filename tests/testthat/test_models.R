context('clModels')

kml2 = m1 = cluslong(clMethodTestKML(nClusters=2), testLongData)
kml3 = cluslong(clMethodTestKML(nClusters=3), testLongData)
kml4 = cluslong(clMethodTestKML(nClusters=4), testLongData)
gmm = m2 = cluslong(clMethodGMM(nClusters=2), testLongData)

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

test_that('as.data.table', {
  clModels() %>%
    as.data.table %>%
    expect_named(c('.name', '.method')) %T>%
    {expect_equal(nrow(.), 0)}

  clModels(m1) %>%
    as.data.table %>%
    expect_length(length(getMethod(m1)) + 2) %>%
    expect_named(c('.name', '.method', names(getMethod(m1))))

  clModels(m1) %>%
    as.data.table %>%
    expect_length(length(getMethod(m1)) + 2) %>%
    expect_named(c('.name', '.method', names(getMethod(m1))))
})

test_that('subset', {
  subset(clModels(), .method == 'kml') %>%
    expect_is('clModels') %>%
    expect_length(0)

  models = clModels(group=c(kml2, gmm), kml3, kml4)

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
})

test_that('metric', {
  metric(clModels(), 'BIC') %>%
    expect_is('data.frame') %T>%
    {expect_equal(nrow(.), 0)}

  models = clModels(group=c(kml2, gmm), kml3, kml4)
  metric(models, 'BIC') %>%
    expect_is('data.frame') %>%
    expect_named(c('.name', '.method', 'BIC'))

  metric(models, c('AIC', 'BIC')) %>%
    expect_is('data.frame') %>%
    expect_named(c('.name', '.method', 'AIC', 'BIC'))

  as.list(models) %>%
    metric(c('AIC', 'BIC')) %>%
    expect_is('data.frame')
})

test_that('plotMetric', {
  models = clModels(group=c(kml2, gmm), kml3, kml4)
  plotMetric(models, name='BIC', subset=.method == 'kml') %>%
    expect_is('gg')

  plotMetric(models, name=c('logLik', 'BIC'), subset=.method == 'kml') %>%
    expect_is('gg')

  plotMetric(models, name=c('logLik', 'BIC'), by='nClusters', group=character()) %>%
    expect_is('gg')
})
