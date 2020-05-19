context('clModel')

model = cluslong(clMethodTestKML(), data=testLongData)

test_that('clusterAssignments', {
  clusterAssignments(model) %>%
    expect_is('factor') %T>%
    {expect_equal(nlevels(.), nClusters(model))}
})

test_that('make.clusterAssignments', {
  refFac = clusterAssignments(model)

  make.clusterAssignments(model, refFac) %>%
    expect_equal(refFac)

  make.clusterAssignments(model, as.integer(refFac)) %>%
    expect_equal(refFac)

  make.clusterAssignments(model, as.numeric(refFac)) %>%
    expect_equal(refFac)

  make.clusterAssignments(model, as.character(refFac)) %>%
    expect_equal(refFac)

  make.clusterAssignments(model, factor(refFac, levels=rev(levels(refFac)))) %>%
    expect_equal(refFac)
})

test_that('make.clusterIndices', {
  refFac = clusterAssignments(model)
  refIdx = as.integer(refFac)

  make.clusterIndices(model, refFac) %>%
    expect_equal(refIdx)

  make.clusterIndices(model, as.integer(refFac)) %>%
    expect_equal(refIdx)

  make.clusterIndices(model, as.numeric(refFac)) %>%
    expect_equal(refIdx)

  make.clusterIndices(model, as.character(refFac)) %>%
    expect_equal(refIdx)

  make.clusterIndices(model, factor(refFac, levels=rev(levels(refFac)))) %>%
    expect_equal(refIdx)
})

test_that('metrics', {
  expect_length(metric(model, character()), 0)

  metric(model, 'BIC') %>%
    expect_is('numeric') %>%
    expect_named('BIC')

  metric(model, '@undefined') %>%
    expect_is('numeric') %>%
    expect_named('@undefined') %>%
    expect_equal(c('@undefined'=NA*0))

  metric(model, c('AIC', '@undefined', 'BIC')) %>%
    expect_is('numeric') %>%
    expect_named(c('AIC', '@undefined', 'BIC')) %T>%
    {expect_equal(unname(.[2]), NA*0)}

  externalMetric(model, model, 'Jaccard') %>%
    expect_is('numeric') %>%
    expect_named('Jaccard')
})