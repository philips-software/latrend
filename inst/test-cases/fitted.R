# Test whether the resulting lcModel supports extracting fitted values.
# This is tested by validating the output of:
# * fitted(),
# * residuals()
####

# Prepare data ####
dataset2 = dataset[Cluster %in% unique(Cluster)[1:2]]
refClusters = dataset2[, first(Cluster), keyby = Id]$V1
S = uniqueN(dataset2$Id)
trajNames = unique(dataset2$Id)
moments = unique(dataset2$Time)
clusSizes = dataset2[, uniqueN(Id), keyby = Cluster]$V1
M = uniqueN(dataset2$Time)

# Fit model ####
testData = subset(dataset2, select = c('Id', 'Time', 'Value')) %>%
  setnames(c('Id', 'Time', 'Value'), c('Traj', 'Moment', 'Y'))
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
test('converged', !isFALSE(converged(model) > 0))
test('nClusters', nClusters(model), 2)
test(
  'trajectoryAssignments.uniqueN',
  uniqueN(trajectoryAssignments(model)),
  2L,
  text = 'expecting each cluster to have at least some assigned trajectories'
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

if (rlang::is_installed('ggplot2')) {
  test('plotFittedTrajectories', plotFittedTrajectories(model), runOnly = TRUE)
}
