context('cluslong')

test_that('default', {
  model = cluslong(clMethodTestKML(), data=testLongData) %>%
    expect_is('clModel')

  expect_equal(deparse(getCall(model)$data), 'testLongData')
  expect_equal(deparse(getCall(model)$envir), 'NULL')
})

test_that('method var', {
  kml = clMethodTestKML()
  cluslong(kml, data=testLongData) %>%
    expect_is('clModel')
})

test_that('overwritten argument', {
  model = cluslong(clMethodTestKML(), data=testLongData, nClusters=1)

  expect_equal(nClusters(model), 1)
  expect_equal(getMethod(model)$nClusters, 1)
  expect_equal(getCall(model)$method$nClusters, 1)
})

test_that('method var with overwritten argument', {
  kml = clMethodTestKML()
  model = cluslong(kml, data=testLongData, nClusters=1)

  expect_equal(nClusters(model), 1)
  expect_equal(getMethod(model)$nClusters, 1)
  expect_equal(getCall(model)$method$nClusters, 1)
})

test_that('new method arguments', {
  model = cluslong(clMethodTestKML(), data=testLongData, test=2)

  expect_equal(getMethod(model)$test, 2)
})

test_that('subset', {
  model = cluslong(clMethodTestKML(), data=testLongData[Time < .5]) %>%
    expect_is('clModel')

  expect_equal(deparse(getCall(model)$data), 'testLongData[Time < 0.5]')
})

test_that('data call', {
  model = cluslong(clMethodTestKML(), data=as.data.table(testLongData)) %>%
    expect_is('clModel')

  expect_equal(deparse(getCall(model)$data), 'as.data.table(testLongData)')
})

test_that('specify id and time with matrix input', {
  mat = dcastRepeatedMeasures(testLongData)
  model = cluslong(clMethodTestKML(), id='Device', time='Observation', data=mat) %>%
    expect_is('clModel')

  expect_equal(deparse(getCall(model)$data), 'mat')
})

test_that('envir', {
  kml = clMethodKML(nClusters=a, nbRedrawing=1, maxIt=10)
  e = list2env(list(a = 1))

  model = cluslong(kml, data=testLongData, envir=e) %>%
    expect_is('clModel')

  expect_equal(nClusters(model), 1)
})

test_that('data.frame input', {
  df = as.data.frame(testLongData)
  model = cluslong(clMethodTestKML(), data=df) %>%
    expect_is('clModel')
})

test_that('matrix input', {
  mat = dcastRepeatedMeasures(testLongData)
  model = cluslong(clMethodTestKML(), data=mat) %>%
    expect_is('clModel')
})

test_that('custom id and time', {
  nameData = copy(testLongData) %>%
    setnames(c('Id', 'Time'), c('Device', 'Observation'))
  model = cluslong(clMethodTestKML(), id='Device', time='Observation', data=nameData) %>%
    expect_is('clModel')

  expect_equal(deparse(getCall(model)$data), 'nameData')
})

test_that('id with NA', {
  set.seed(1)
  naData = copy(testLongData) %>%
    .[sample(.N, 10), Id := NA]

  expect_error(cluslong(clMethodTestKML(), data=naData))
})

test_that('factor id', {
  facData = copy(testLongData) %>%
    .[, Id := factor(Id)]

  model = cluslong(clMethodTestKML(), data=facData) %>%
    expect_is('clModel')

  expect_equal(ids(model), levels(facData$Id))
})

test_that('factor id, out of order', {
  facData = copy(testLongData) %>%
    .[, Id := factor(Id, levels=rev(unique(Id)))]

  model = cluslong(clMethodTestKML(), data=facData) %>%
    expect_is('clModel')

  expect_equal(ids(model), levels(facData$Id))
})

test_that('factor id with empty levels', {
  facData = copy(testLongData) %>%
    .[, Id := factor(Id, levels=seq(0, uniqueN(Id) + 1))]

  model = cluslong(clMethodTestKML(), data=facData) %>%
    expect_is('clModel')
})

test_that('id with NA', {
  naData = copy(testLongData) %>%
    .[Id == 1, Id := NA]

  expect_error(cluslong(clMethodTestKML(), data=naData))
})

test_that('shuffled data', {
  set.seed(1)
  shufData = copy(testLongData) %>%
    .[sample(.N)]

  cluslong(clMethodTestKML(), data=shufData) %>%
    expect_is('clModel')
})

test_that('data with NA observations', {
  set.seed(1)
  naData = copy(testLongData) %>%
    .[sample(.N, 10), Value := NA]

  cluslong(clMethodTestKML(), data=naData) %>%
    expect_is('clModel')
})

test_that('data with Inf observations', {
  set.seed(1)
  infData = copy(testLongData) %>%
    .[sample(.N, 10), Value := Inf]

  expect_error(cluslong(clMethodTestKML(), data=infData))
})

test_that('data with missing observations', {
  set.seed(1)
  naData = copy(testLongData) %>%
    .[sample(.N, 100)]

  cluslong(clMethodTestLcmmGBTM(), data=naData) %>%
    expect_is('clModel')

  expect_error(cluslong(clMethodTestKML(), data=naData))
})

