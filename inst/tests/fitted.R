# Test whether the resulting lcModel supports extracting fitted values.
# This is tested by validating the output of:
# * fitted(),
# * residuals()
####

# Prepare data ####
S = 25
M = 4
trajNames = paste0('S', seq_len(S * 2))
moments = seq_len(M)

testData = data.table(
  Traj = rep(trajNames, each = M) %>% factor(levels = trajNames),
  Moment = rep(moments, S * 2),
  Y = c(rnorm(S * M, mean = -5), rnorm(S * M, mean = 5)),
  Class = rep(LETTERS[1:2], each = S * M)
)
refClusters = rep(LETTERS[1:2], each = S)

# Fit model ####
m = make.lcMethod(id = 'Traj', time = 'Moment', response = 'Y', nClusters = 2)

model = latrend(m, data = testData)

refModel = lcModelPartition(
  testData,
  response = 'Y',
  time = 'Moment',
  id = 'Traj',
  trajectoryAssignments = refClusters
)

# Tests ####
test('converged', converged(model) > 0)
test('nClusters', nClusters(model), 2)
test(
  'trajectoryAssignments.ref',
  externalMetric(model, refModel, 'adjustedRand') == 1,
  check.attributes = FALSE,
  text = 'does trajectory assignment match the reference?'
)

test('fitted', fitted(model), runOnly = TRUE)
test('fitted.length', length(fitted(model)), nrow(testData))
test('fitted.clusters', ncol(fitted(model, clusters = NULL)), 2)
test('fittedTrajectories', fittedTrajectories(model), runOnly = TRUE)
test('residuals', residuals(model), runOnly = TRUE)
test('residuals.length', length(residuals(model)), nrow(testData))

test('predictForCluster.newdata', {
  predClus = predictForCluster(
    model,
    newdata = data.frame(Moment = moments[c(1,3)]),
    cluster = 'A'
  )
}, runOnly = TRUE)
test('predictForCluster.numeric', is.numeric(predClus))
test('predictForCluster.length', length(predClus), 2)

test('predictForCluster.cluster=A', {
  predClusNull = predictForCluster(model, cluster = 'A')
}, runOnly = TRUE)
test('predictForCluster.cluster=A&newdata=NULL', {
  predClusNull2 = predictForCluster(model, newdata = NULL, cluster = 'A')
}, runOnly = TRUE)
test('predictForCluster.cluster=A&newdata=NULL.compare', predClusNull, predClusNull2)
test('predictForCluster.cluster=A&newdata=model.data', {
  predClusFitted = predictForCluster(model, newdata = model.data(model), cluster = 'A')
}, runOnly = TRUE)
test('predictForCluster.cluster=A&newdata=model.data.compare', predClusNull2, predClusFitted)

test('predict', predict(model, newdata = NULL), runOnly = TRUE)

test('plotFittedTrajectories', plotFittedTrajectories(model), runOnly = TRUE)
