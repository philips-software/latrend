context('lcModel without data')
rngReset()

setClass('lcModelWithoutData', contains = 'lcModel')

model = new('lcModelWithoutData',
  method = new('lcMethod'),
  id = 'Subject',
  time = 'Month',
  response = 'Meas',
  data = NULL,
  model = list(),
  clusterNames = LETTERS[1:3]
)

test_that('responseVariable', {
  out = responseVariable(model)
  expect_is(out, 'character')
})

test_that('idVariable', {
  out = idVariable(model)
  expect_is(out, 'character')
})

test_that('timeVariable', {
  out = timeVariable(model)
  expect_is(out, 'character')
})

test_that('getName', {
  out = getName(model)
  expect_is(out, 'character')
})

test_that('getShortName', {
  out = getShortName(model)
  expect_is(out, 'character')
})

test_that('getLabel', {
  out = getLabel(model)
  expect_is(out, 'character')
})

test_that('model.data', {
  expect_warning({
    out = model.data(model)
  })
  expect_null(out)
})

test_that('time', {
  expect_error(suppressWarnings(time(model)))
})

test_that('postprob', {
  expect_warning({
    out = postprob(model)
  })

  expect_true(nrow(out) == 0)
})

test_that('clusterProportions', {
  expect_error(suppressWarnings(clusterProportions(model)))
})

test_that('trajectoryAssignments', {
  out = trajectoryAssignments(model)
  expect_length(out, 0)
})

test_that('fitted', {
  expect_warning({
    out = fitted(model)
  })

  expect_length(out, 0)
})

test_that('residuals', {
  expect_warning({
    out = residuals(model)
  })

  expect_length(out, 0)
})
