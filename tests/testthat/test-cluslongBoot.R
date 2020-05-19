context('cluslongBoot')

test_that('cluslongBoot', {
  models = cluslongBoot(clMethodTestKML(), data=testLongData, samples=3, seed=1) %>%
    expect_is('clModels') %>%
    expect_length(3)

  # test if data bootstrap sample calls are correct
  expect_equal(deparse(getCall(models[[1]])$data), 'bootSample(testLongData, "Id", 1140350788L)')
  expect_equal(deparse(getCall(models[[2]])$data), 'bootSample(testLongData, "Id", 312928385L)')
  expect_equal(deparse(getCall(models[[3]])$data), 'bootSample(testLongData, "Id", 866248189L)')
})

test_that('cluslongBoot without seed', {
  cluslongBoot(clMethodTestKML(), data=testLongData, samples=2) %>%
    expect_is('clModels') %>%
    expect_length(2)
})

test_that('cluslongBoot with method var', {
  kml = clMethodTestKML()
  cluslongBoot(kml, data=testLongData, samples=2) %>%
    expect_is('clModels') %>%
    expect_length(2)
})

test_that('cluslongBoot with single sample', {
  cluslongBoot(clMethodTestKML(), data=testLongData, samples=1) %>%
    expect_is('clModels') %>%
    expect_length(1)
})
