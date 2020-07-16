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


test_that('meltRepeatedMeasures', {
  m = matrix(1:12, nrow=3)
  meltRepeatedMeasures(m, response = 'Value') %>%
    expect_is('data.frame') %>%
    expect_named(c('Id', 'Time', 'Value')) %T>%
    {expect_equal(nrow(.), length(m))} %T>%
    {expect_is(.$Id, c('integer', 'factor'))} %T>%
    {expect_is(.$Time, 'numeric')} %T>%
    {expect_is(.$Value, 'numeric')} %T>%
    {expect_equal(.$Value, as.numeric(t(m)))}
})

test_that('meltRepeatedMeasures with non-numeric colnames', {
  m = matrix(1:12, nrow=3)
  colnames(m) = paste0('Obs', 1:4)

  suppressWarnings({
    meltRepeatedMeasures(m, response = 'Value') %>%
      expect_is('data.frame') %>%
      expect_named(c('Id', 'Time', 'Value')) %T>%
      {expect_equal(nrow(.), length(m))} %T>%
      {expect_is(.$Id, c('integer', 'factor'))} %T>%
      {expect_is(.$Time, 'numeric')} %T>%
      {expect_is(.$Value, 'numeric')} %T>%
      {expect_equal(.$Value, as.numeric(t(m)))}
  })
})

test_that('dcastRepeatedMeasures', {
  m = matrix(1:12, nrow=3)
  df = meltRepeatedMeasures(m, response = 'Value')
  dcastRepeatedMeasures(df, response = 'Value') %>%
    expect_is('matrix') %T>%
    {expect_equal(nrow(.), nrow(m))} %T>%
    {expect_equal(ncol(.), ncol(m))} %T>%
    {expect_equal(as.numeric(.), as.numeric(m))}
})
