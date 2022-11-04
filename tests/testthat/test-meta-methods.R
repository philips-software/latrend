method = lcMethodLMKM(Value ~ Assessment, id = 'Traj', time = 'Assessment', nClusters = 2)

test_that('specify converged', {
  metaMethod = lcMetaConverged(method)
  expect_s4_class(metaMethod, 'lcMetaConverged')
  expect_true(has_lcMethod_args(metaMethod, 'maxRep'))
})

test_that('specify converged with maxRep', {
  metaMethod = lcMetaConverged(method, maxRep = 13)
  expect_true(has_lcMethod_args(metaMethod, 'maxRep'))
  expect_equal(metaMethod$maxRep, 13)
})

test_that('meta method class', {
  metaMethod = lcMetaConverged(method)

  expect_output(print(metaMethod), 'encapsulating')
  expect_output(show(metaMethod), 'encapsulating')

  expect_equal(getName(metaMethod), getName(method))
  expect_equal(getShortName(metaMethod), getShortName(method))
  expect_equal(getLabel(metaMethod), getLabel(method))
  expect_equal(getLcMethod(metaMethod), method)

  expect_equal(idVariable(metaMethod), idVariable(method))
  expect_equal(timeVariable(metaMethod), timeVariable(method))
  expect_equal(responseVariable(metaMethod), responseVariable(method))

  expect_equal(
    getCall(metaMethod),
    call('lcMetaConverged', method = getCall(method), maxRep = Inf)
  )
})

test_that('meta converged fit', {
  metaMethod = lcMetaConverged(mRandom)

  expect_no_warning({
    model = latrend(metaMethod, testLongData)
  })
})
