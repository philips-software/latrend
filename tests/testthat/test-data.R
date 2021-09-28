context('data')

test_that('osa structure', {
  data(OSA.adherence)

  expect_is(OSA.adherence, 'data.frame')
  expect_named(OSA.adherence, c('Patient', 'Biweek', 'MaxDay', 'UsageHours', 'Group'))
  expect_is(OSA.adherence$Patient, 'factor')
  expect_is(OSA.adherence$Biweek, 'integer')
  expect_is(OSA.adherence$MaxDay, 'integer')
  expect_is(OSA.adherence$UsageHours, 'numeric')
  expect_is(OSA.adherence$Group, 'factor')
})

test_that('osa content', {
  expect_gte(min(OSA.adherence$UsageHours), 0)
  expect_true(noNA(OSA.adherence$Patient))
  expect_true(noNA(OSA.adherence$Biweek))
  expect_true(noNA(OSA.adherence$UsageHours))
  expect_equal(min(OSA.adherence$Biweek), 1)
  expect_equal(max(OSA.adherence$Biweek), 26)
})
