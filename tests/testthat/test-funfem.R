context('funfem')

library(funFEM)
data(CanadianWeather)
femData = CanadianWeather$dailyAv[,,'Temperature.C'] %>% t

test_that('default', {
  suppressWarnings({
    model = cluslong(clMethodFunFEM(), femData) %>%
      expect_valid_clModel()
  })
})

test_that('many clusters', {
  suppressWarnings({
    model = cluslong(clMethodFunFEM(nClusters=4), femData) %>%
      expect_valid_clModel()
  })
})

test_that('testLongData', {
  suppressWarnings({
    model = cluslong(clMethodFunFEM(), testLongData) %>%
      expect_valid_clModel()
  })
})
