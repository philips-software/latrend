logging::setLevel(loglevels['FATAL'])

mixt_file = file.path('..', '..', 'MixTVEM.R')
if(file.exists(mixt_file)) {
  source(mixt_file)
}

clMethodTestKML = function(...) {
  clMethodKML(nRuns=1, maxIter=10, ...)
}

clMethodTestGMM = function(...) {
  clMethodGMM(maxIter=10, ...)
}

clMethodTestGBTM = function(...) {
  clMethodGBTM(maxIter=10, ...)
}

expect_valid_clModel = function(object) {
  expect_s4_class(object, 'clModel')

  expect_is(getCall(object), 'call')
  expect_is(model.data(object), 'data.frame')
  expect_is(getName(object), 'character')
  expect_is(getName0(object), 'character')
  expect_is(idVariable(object), 'character')
  expect_is(timeVariable(object), 'character')
  expect_is(responseVariable(object), 'character')
  expect_is(clusterTrajectories(object), 'data.frame')
  expect_is(trajectories(object), 'data.frame')
  expect_true(is.ggplot(plot(object)))
  expect_is(coef(object), 'numeric')
  expect_is(residuals(object, clusters=clusterAssignments(object)), c('NULL', 'numeric'))
  expect_is(residuals(object, clusters=NULL), c('NULL', 'matrix'))
  expect_is(converged(object), c('logical', 'numeric', 'integer'))
  expect_is(fitted(object, clusters=clusterAssignments(object)), c('NULL', 'numeric'))
  expect_is(fitted(object, clusters=NULL), c('NULL', 'matrix'))

  expect_true(is.count(nClusters(object)))
  clusterNames(object) %>%
    expect_is('character') %>%
    expect_length(nClusters(object))
  expect_is(clusterNames(object, factor=TRUE), 'factor')

  clusterAssignments(object) %>%
    expect_is('factor') %>%
    expect_length(nIds(object)) %>%
    as.integer %T>%
    {expect_gte(min(.), 1)} %T>%
    {expect_lte(max(.), nIds(object))}

  postprob(object) %>%
    expect_is('matrix') %T>%
    {expect_true(all(is.finite(.)))} %T>%
    {expect_equal(ncol(.), nClusters(object))} %T>%
    {expect_equal(nrow(.), nIds(object))} %T>%
    {expect_gte(min(.), 0)} %T>%
    {expect_lte(max(.), 1)}

  expect_is(summary(object), 'clSummary')
  expect_output(print(object))

  return(object)
}
