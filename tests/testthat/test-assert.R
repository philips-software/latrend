context('assert')
rngReset()

m1 = latrend(mTest, data = testLongData)
m2 = latrend(mRandomTest, data = testLongData)

test_that('is_named', {
  expect_true(is_named(setNames(1:2, LETTERS[1:2])))
  expect_true(is_named(setNames(character(), character()))) #empty named char vector

  expect_false(is_named(NULL))
  expect_false(is_named(character()))
  expect_false(is_named(character(2)))
  expect_false(is_named(NA))
  expect_false(is_named(1:2))
  expect_false(is_named(factor(1:2, levels = LETTERS[1:2])))
})

test_that('is_newdata', {
  expect_true(is_newdata(NULL))
  expect_true(is_newdata(data.frame()))

  expect_false(is_newdata(numeric()))
  expect_false(is_newdata(NA))
  expect_false(is_newdata(numeric(2)))
  expect_false(is_newdata(list(1, 2)))
})

test_that('is_at', {
  expect_true(is_at(.1))
  expect_true(is_at(0:1))
  expect_true(is_at(c(0, 0)))
  expect_true(is_at(c(-1)))

  expect_false(is_at('0'))
  expect_false(is_at(c(1, NA)))
  expect_false(is_at(c(1, Inf)))
})

test_that('has_same_ids', {
  expect_true(has_same_ids(m1, m2))
})

test_that('has_same_modelData', {
  expect_true(has_same_modelData(m1, m2))
})

test_that('is_class_defined', {
  expect_true(is_class_defined(m1))
})

test_that('has_lcMethod_args', {
  expect_true(has_lcMethod_args(mTest, 'response'))
  expect_false(has_lcMethod_args(mTest, '..missing'))
})

test_that('is_valid_postprob', {
  expect_true(is_valid_postprob(matrix(1)))
  expect_true(is_valid_postprob(matrix(c(0, 1, 1, 0), nrow = 2)))

  expect_false(is_valid_postprob(NULL))
  expect_false(is_valid_postprob(1))
  expect_false(is_valid_postprob(matrix('1')))
  expect_false(is_valid_postprob(matrix(.5)))

  expect_false(is_valid_postprob(matrix(c(0, 1, 0, 0), nrow = 2))) # a row with sum < 1
  expect_false(is_valid_postprob(matrix(c(1, 1, 1, 0), nrow = 2))) # a row sum > 1
  expect_false(is_valid_postprob(matrix(c(-1, 1, 2, 0), nrow = 2))) # negative entries
})
