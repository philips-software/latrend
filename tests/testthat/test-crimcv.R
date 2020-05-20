context('crimcv')

library(crimCV)
data(TO1adj)
subTO1adj = TO1adj[1:100, seq(1, ncol(TO1adj), by=2)]

test_that('default tau', {
  suppressWarnings({
    model = cluslong(clMethodTestCrimCVt(), subTO1adj) %>%
      expect_valid_clModel()
  })

  plot(model, what='nu') %>%
    expect_is('ggplot')
})

test_that('default', {
  suppressWarnings({
    model = cluslong(clMethodTestCrimCV(), subTO1adj) %>%
      expect_valid_clModel()
  })

  plot(model, what='nu') %>%
    expect_is('ggplot')
})

test_that('many clusters', {
  suppressWarnings({
    model = cluslong(clMethodTestCrimCVt(nClusters=4), subTO1adj) %>%
      expect_valid_clModel()
  })
})