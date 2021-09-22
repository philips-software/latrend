context('logging')

test_that('Verbose', {
  expect_is(as.Verbose(NULL), 'Verbose')
  expect_is(as.Verbose(0), 'Verbose')
  expect_is(as.Verbose('info'), 'Verbose')
  expect_is(as.Verbose(as.Verbose(0)), 'Verbose')
})
