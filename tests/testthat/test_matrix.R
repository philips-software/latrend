context('matrix')

test_that('rowColumns', {
  m = matrix(1:12, ncol=3)
  expect_error(rowColumns(m, 1))
  expect_equal(rowColumns(m, c(1,1,1,1)), m[,1])
  expect_equal(rowColumns(m, c(1,2,3,1)), m[cbind(1:4, c(1,2,3,1))])
  expect_error(rowColumns(m, c(1,2,NA,3)))
  expect_error(rowColumns(m, c(1,2,Inf,3)))
  expect_error(rowColumns(m, c(1,4,1,1)))
  expect_error(rowColumns(m, c(0,1,1,1)))
})
