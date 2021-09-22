context('timing')

test_that('.formatElapsedSeconds', {
  txt = .formatElapsedSeconds(1)
  expect_true(is.character(txt))
  expect_length(txt, 1)
  expect_gt(nchar(txt), 2)

  expect_false(startsWith(.formatElapsedSeconds(0), '-'))
  expect_true(startsWith(.formatElapsedSeconds(NaN), 'NaN'))
  expect_true(startsWith(.formatElapsedSeconds(NA*0), 'NA'))

  expect_false(.formatElapsedSeconds(1e4, unit = 'secs') == .formatElapsedSeconds(1e4, unit = 'hours'))
  expect_gt(nchar(.formatElapsedSeconds(1e4, unit = 'secs')), nchar(.formatElapsedSeconds(1e4, unit = 'hours')))

  expect_warning(.formatElapsedSeconds(-1))
  suppressWarnings({
    expect_equal(.formatElapsedSeconds(-1), .formatElapsedSeconds(0))
  })
})

test_that('tictoc', {
  a = .tic()
  secs = .toc(a)

  expect_true(is.numeric(secs))
  expect_length(secs, 1)
  expect_gte(secs, 0)

  secsText = .tocText(a)
  expect_true(is.character(secsText))
  expect_length(secsText, 1)
})
