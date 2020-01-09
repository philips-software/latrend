context('model list')

m1 = cluslong(clMethodKML(), testLongData)
m2 = cluslong(clMethodGMM(), testLongData)

test_that('as', {
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
