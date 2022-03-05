context('KML model')
skip_if_not_installed('kml')
rngReset()
tests = c(DEFAULT_LATREND_TESTS)

make.kml = function(...) {
  lcMethodKML(..., nbRedrawing = 1, maxIt = 10, seed = 1)
}

test_that('kml', {
  expect_true({
    test.latrend('lcMethodKML', instantiator = make.kml, tests = tests)
  })
})

# test_that('predictPostprob', {
#   model = latrend(lcMethodTestKML(), testLongData)
#   testData = testLongData[Traj %in% unique(Traj)[1:3]]
#   pp = predictPostprob(model, newdata = testData)
#   expect_true(is_valid_postprob(pp, model))
# })

test_that('cld.Rdata not present', {
  expect_false(file.exists('cld.Rdata'))
})
