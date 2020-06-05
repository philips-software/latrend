mixt_file = file.path('..', '..', 'MixTVEM.R')
if(file.exists(mixt_file)) {
  source(mixt_file)
}

options(cluslong.id='Id',
        cluslong.time='Time',
        cluslong.verbose=R.utils::Verbose())

clMethodTestKML = function(...) {
  clMethodKML(nbRedrawing=1, maxIt=10, ..., seed=1)
}

clMethodTestLcmmGMM = function(...) {
  clMethodLcmmGMM(maxiter=10, ..., seed=1)
}

clMethodTestLcmmGBTM = function(...) {
  clMethodLcmmGBTM(maxiter=10, ..., seed=1)
}

clMethodTestFlexmixGBTM = function(...) {
  clMethodFlexmixGBTM(..., control=list(iter.max=1, tolerance=1e-2), seed=1)
}

clMethodTestCrimCV = function(...) {
  clMethodCrimCV(..., model='ZIP', dpolyp=2, dpolyl=1, init=2, seed=1)
}

clMethodTestCrimCVt = function(...) {
  clMethodCrimCV(..., model='ZIPt', dpolyp=2, init=2, seed=1)
}

clMethodTestLongclust = function(...) {
  clMethodLongclust(modelSubset='VVA', gaussian=TRUE, ..., seed=1)
}

clMethodTestLongclustT = function(...) {
  clMethodLongclust(modelSubset='VEI', gaussian=FALSE, ..., seed=1)
}

clMethodTestMixtoolsNPRM = function(...) {
  clMethodMixtoolsNPRM(maxiter=10, eps=1e-04, seed=1)
}

clMethodTestMixtoolsGMM = function(...) {
  clMethodMixtoolsGMM(epsilon=1e-02, ..., seed=1)
}

expect_valid_clModel = function(object) {
  expect_s4_class(object, 'clModel')

  getCall(object) %>%
    expect_is('call')
  model.data(object) %>%
    expect_is('data.frame')
  getName(object) %>%
    expect_is('character')
  getShortName(object) %>%
    expect_is('character')
  idVariable(object) %>%
    expect_is('character')
  timeVariable(object) %>%
    expect_is('character')
  responseVariable(object) %>%
    expect_is('character')
  coef(object) %>%
    expect_is(c('numeric', 'matrix', 'list', 'NULL'), label='coef')
  converged(object) %>%
    expect_is(c('logical', 'numeric', 'integer'), label='converged')
  nClusters(object) %T>%
    {expect_true(is.count(.))}
  clusterNames(object) %>%
    expect_is('character') %>%
    expect_length(nClusters(object))

  # Posterior
  pp = postprob(object)
  expect_is(pp, 'matrix', label='postprob')
  expect_true(all(is.finite(pp)))
  expect_equal(ncol(pp), nClusters(object))
  expect_equal(nrow(pp), nIds(object))
  expect_gte(min(pp), 0)
  expect_lte(max(pp), 1)

  clus = clusterAssignments(object)
  expect_is(clus, 'factor')
  expect_length(clus, nIds(object))
  expect_gte(min(as.integer(clus)), 1)
  expect_lte(max(as.integer(clus)), nIds(object))

  # Predict
  if(!is(object, 'clModelCustom')) {
    # cluster-specific prediction
    pred = predict(object, newdata=data.frame(Cluster='A', Time=time(object)[c(1,3)]))
    expect_is(pred, 'data.frame', info='predictClusterTime')
    expect_true('Fit' %in% names(pred), info='predictClusterTime')
    expect_equal(nrow(pred), 2, info='predictClusterTime')

    # prediction for all clusters; list of data.frames
    pred2 = predict(object, newdata=data.frame(Time=time(object)[c(1,3)]))
    expect_is(pred2, 'list', info='predictTime')
    expect_length(pred2, nClusters(object))
    expect_true('Fit' %in% names(pred2$A), info='predictTime')

    # id-specific prediction for a specific cluster; data.frame
    pred3 = predict(object, newdata=data.frame(Cluster=rep('A', 4),
                                       Id=c(ids(object)[c(1,1,2)], tail(ids(object), 1)),
                                       Time=c(time(object)[c(1,3,1,1)])))
    expect_is(pred3, 'data.frame', info='predictClusterIdTime')
    expect_true('Fit' %in% names(pred3), info='predictClusterIdTime')
    expect_equal(nrow(pred3), 4, info='predictClusterIdTime')

    # id-specific prediction for all clusters; list of data.frames
    pred4 = predict(object, newdata=data.frame(Id=c(ids(object)[c(1,1,2)], tail(ids(object), 1)),
                                       Time=c(time(object)[c(1,3,1,1)])))
    expect_is(pred4, 'list', info='predictIdTime')
    expect_length(pred4, nClusters(object))
    expect_true('Fit' %in% names(pred4$A), info='predictIdTime')

    fitted(object, clusters=clusterAssignments(object)) %>%
      expect_is(c('NULL', 'numeric'), info='fittedClusters')
    fitted(object, clusters=NULL) %>%
      expect_is(c('NULL', 'matrix'), info='fittedNull')

    predNul = predict(object, newdata=NULL)
    expect_is(predNul, 'list', info='predictNull')
    expect_length(predNul, nClusters(object))
    expect_true('Fit' %in% names(predNul$A), info='predictNull')

    residuals(object, clusters=clusterAssignments(object)) %>%
      expect_is(c('NULL', 'numeric'), label='residuals')
    residuals(object, clusters=NULL) %>%
      expect_is(c('NULL', 'matrix'), label='residuals')
  }


  # Derivative predict
  clusterTrajectories(object) %>%
    expect_is('data.frame', label='clusterTrajectories')
  trajectories(object) %>%
    expect_is('data.frame', label='trajectories')

  expect_true(is.ggplot(plot(object)))

  # Misc
  summary(object) %>%
    expect_is('clSummary')
  expect_output(print(object))

  return(object)
}
