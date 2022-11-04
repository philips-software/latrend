method = lcMethodLMKM(Value ~ Assessment, id = 'Traj', time = 'Assessment', nClusters = 2)

test_that('class', {
  metaMethod = lcFitConverged(method)

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
    call('lcFitConverged', method = getCall(method), maxRep = Inf)
  )
})
