logging::setLevel(loglevels['FATAL'])

expect_valid_clModel = function(object) {
  expect_s4_class(object, 'clModel')

  expect_is(getCall(object), 'call')
  expect_is(modelData(object), 'data.frame')
  expect_is(getName(object), 'character')
  expect_is(getName0(object), 'character')
  expect_is(getIdName(object), 'character')
  expect_is(getTimeName(object), 'character')
  expect_is(getResponseName(object), 'character')
  expect_is(clusterTrajectories(object), 'data.frame')
  expect_is(trajectories(object), 'data.frame')
  expect_true(is.ggplot(plot(object)))
  expect_is(coef(object), 'numeric')
  expect_is(residuals(object), 'numeric')
  expect_is(converged(object), 'logical')
  expect_false(is.null(getModel(object)))
  fit = fitted(object)
  expect_is(fit, 'numeric')

  expect_true(is.count(nClus(object)))
  clusterNames(object) %>%
    expect_is('character') %>%
    expect_length(nClus(object))
  expect_is(clusterNames(object, factor=TRUE), 'factor')

  clusterAssignments(object) %>%
    expect_is('factor') %>%
    expect_length(nIds(object)) %>%
    as.integer %T>%
    {expect_gte(min(.), 1)} %T>%
    {expect_lte(max(.), nIds(object))}

  postprob(object) %>%
    expect_is('matrix') %T>%
    {expect_equal(ncol(.), nClus(object))} %T>%
    {expect_equal(nrow(.), nIds(object))} %T>%
    {expect_gte(min(.), 0)} %T>%
    {expect_lte(max(.), 1)}

  return(object)
}