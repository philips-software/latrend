context('funfem')

library(funFEM)
data(CanadianWeather)
femData = CanadianWeather$dailyAv[,,'Temperature.C'] %>% t

test_that('default', {
  suppressWarnings({
    model = latrend(lcMethodFunFEM(), femData) %>%
      expect_valid_lcModel()
  })
})

test_that('many clusters', {
  suppressWarnings({
    model = latrend(lcMethodFunFEM(nClusters=4), femData) %>%
      expect_valid_lcModel()
  })
})

test_that('testLongData', {
  suppressWarnings({
    model = latrend(lcMethodFunFEM(), testLongData) %>%
      expect_valid_lcModel()
  })
})
