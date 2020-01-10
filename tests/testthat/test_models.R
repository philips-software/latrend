context('model list')

m1 = cluslong(clMethodKML(), testLongData)
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