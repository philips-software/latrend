mixt_file = file.path('..', '..', 'MixTVEM.R')
if(file.exists(mixt_file)) {
  source(mixt_file)
}

options(cluslong.id='Id',
        cluslong.time='Time',
        cluslong.verbose=R.utils::Verbose())

clMethodTestKML = function(...) {
  clMethodKML(nRuns=1, maxIter=10, ...)
}

clMethodTestLcmmGMM = function(...) {
  clMethodLcmmGMM(maxiter=10, ...)
}

clMethodTestLcmmGBTM = function(...) {
  clMethodLcmmGBTM(maxiter=10, ...)
}

clMethodTestFlexmixGBTM = function(...) {
  clMethodFlexmixGBTM(..., control=list(iter.max=1, tolerance=1e-2))
}

clMethodTestCrimCV = function(...) {
  clMethodCrimCV(..., model='ZIP', dpolyp=2, dpolyl=1, init=2, seed=1)
}

clMethodTestCrimCVt = function(...) {
  clMethodCrimCV(..., model='ZIPt', dpolyp=2, init=2, seed=1)
}

clMethodTestLongclust = function(...) {
  clMethodLongclust(modelSubset='VVA', gaussian=TRUE, ...)
}

clMethodTestLongclustT = function(...) {
  clMethodLongclust(modelSubset='VEI', gaussian=FALSE, ...)
}

clMethodTestMixtoolsNPRM = function(...) {
  clMethodMixtoolsNPRM(maxiter=10, eps=1e-04)
}

clMethodTestMixtoolsGMM = function(...) {
  clMethodMixtoolsGMM(epsilon=1e-02, ...)
}

expect_valid_clModel = function(object) {
  expect_s4_class(object, 'clModel')

  getCall(object) %>%
    expect_is('call')
  model.data(object) %>%
    expect_is('data.frame')
  getName(object) %>%
    expect_is('character')
  getName0(object) %>%
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
  postprob(object) %>%
    expect_is('matrix', label='postprob') %T>%
    {expect_true(all(is.finite(.)))} %T>%
    {expect_equal(ncol(.), nClusters(object))} %T>%
    {expect_equal(nrow(.), nIds(object))} %T>%
    {expect_gte(min(.), 0)} %T>%
    {expect_lte(max(.), 1)}

  clusterAssignments(object) %>%
    expect_is('factor') %>%
    expect_length(nIds(object)) %>%
    as.integer %T>%
    {expect_gte(min(.), 1)} %T>%
    {expect_lte(max(.), nIds(object))}

  # Predict
  if(!is(object, 'clModelCustom')) {
    predict(object, newdata=data.frame(Cluster='A', Time=time(object)[c(1,3)])) %>% # cluster-specific prediction
      expect_is('data.frame', info='predictClusterTime') %T>%
      {expect_true('Fit' %in% names(.), info='predictClusterTime')} %T>%
      {expect_equal(nrow(.), 2, info='predictClusterTime')}

    predict(object, newdata=data.frame(Time=time(object)[c(1,3)])) %>% # prediction for all clusters; list of data.frames
      expect_is('list', info='predictTime') %>%
      expect_length(nClusters(object)) %T>%
      {expect_true('Fit' %in% names(.$A), info='predictTime')}

    predict(object, newdata=data.frame(Cluster=rep('A', 4),
                                       Id=c(ids(object)[c(1,1,2)], tail(ids(object), 1)),
                                       Time=c(time(object)[c(1,3,1,1)]))) %>% # id-specific prediction for a specific cluster; data.frame
      expect_is('data.frame', info='predictClusterIdTime') %T>%
      {expect_true('Fit' %in% names(.), info='predictClusterIdTime')} %T>%
      {expect_equal(nrow(.), 4, info='predictClusterIdTime')}

    predict(object, newdata=data.frame(Id=c(ids(object)[c(1,1,2)], tail(ids(object), 1)),
                                       Time=c(time(object)[c(1,3,1,1)]))) %>% # id-specific prediction for all clusters; list of data.frames
      expect_is('list', info='predictIdTime') %>%
      expect_length(nClusters(object)) %T>%
      {expect_true('Fit' %in% names(.$A), info='predictIdTime')}

    fitted(object, clusters=clusterAssignments(object)) %>%
      expect_is(c('NULL', 'numeric'), info='fittedClusters')
    fitted(object, clusters=NULL) %>%
      expect_is(c('NULL', 'matrix'), info='fittedNull')

    predict(object, newdata=NULL) %>%
      expect_is('list', info='predictNull') %>%
      expect_length(nClusters(object)) %T>%
      {expect_true('Fit' %in% names(.$A), info='predictNull')}

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
  plot(object) %T>%
    {expect_true(is.ggplot(.))}

  # Misc
  summary(object) %>%
    expect_is('clSummary')
  expect_output(print(object))

  return(object)
}
