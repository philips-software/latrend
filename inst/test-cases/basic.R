# Tests for the basic process from fitting an easy dataset via the lcMethod class,
# and checking whether the resulting lcModel is valid
####

# Prepare data ####
dataset2 = dataset[Cluster %in% unique(Cluster)[1:2]]
refClusters = dataset2[, first(Cluster), keyby = Id]$V1
S = uniqueN(dataset2$Id)
trajNames = unique(dataset2$Id)
clusSizes = dataset2[, uniqueN(Id), keyby = Cluster]$V1
M = uniqueN(dataset2$Time)

# Fit model ####
testData = copy(dataset2) %>%
  setnames(c('Id', 'Time', 'Value'), c('Traj', 'Moment', 'Y'))

m = make.lcMethod(id = 'Traj', time = 'Moment', response = 'Y', nClusters = 2L)
model = latrend(m, data = testData)

refModel = lcModelPartition(
  testData,
  response = 'Y',
  time = 'Moment',
  id = 'Traj',
  trajectoryAssignments = refClusters
)

# Tests ####
test('getLcMethod', is(getLcMethod(model), class(m)))
test('getName.nzchar', nzchar(getName(model)))
test('getShortName.nzchar', nzchar(getShortName(model)))
test('idVariable', idVariable(model), 'Traj')
test('timeVariable', timeVariable(model), 'Moment')
test('responseVariable', responseVariable(model), 'Y')
test('getCall', is(getCall(model), 'call'))
test('converged', !isFALSE(converged(model) > 0))
test('estimationTime', estimationTime(model) >= 0)
test('nClusters', nClusters(model), 2L)
test('clusterNames', length(clusterNames(model)), 2L)
test('nIds', nIds(model), length(trajNames))
test('ids', ids(model), trajNames)
test('time', time(model), sort(unique(testData$Moment)))

test('postprob', postprob(model), runOnly = TRUE) # tests are done by postprob() generic
test('trajectoryAssignments.nIds', length(trajectoryAssignments(model)), nIds(model))
test(
  'trajectoryAssignments.uniqueN',
  uniqueN(trajectoryAssignments(model)),
  2L,
  text = 'expecting each cluster to have at least some assigned trajectories'
)

if (rlang::is_installed('mclustcomp')) {
  test(
    'trajectoryAssignments.recovery',
    externalMetric(model, refModel, 'adjustedRand') > .99,
    check.attributes = FALSE,
    text = 'does trajectory assignment match the reference?',
    onFail = .Options$latrend.test.checkClusterRecovery
  )
}

test('clusterSizes.len', length(clusterSizes(model)), 2)
test('clusterSizes.total', sum(clusterSizes(model)), S)
test(
  'clusterSizes.recovery',
  clusterSizes(model),
  clusSizes,
  check.attributes = FALSE,
  tolerance = 1,
  onFail = .Options$latrend.test.checkClusterRecovery
)
test('clusterProportions.len', length(clusterProportions(model)), 2)
test(
  'clusterProportions.recovery',
  clusterProportions(model),
  clusSizes / sum(clusSizes),
  check.attributes = FALSE,
  tolerance = .01,
  onFail = .Options$latrend.test.checkClusterRecovery
)

test('model.data', is.data.frame(model.data(model)))
test('model.data.nrow', nrow(model.data(model)) == nrow(testData))
test('nobs', nobs(model), nrow(testData))
test('trajectories', trajectories(model), runOnly = TRUE)

test('clusterTrajectories', clusterTrajectories(model), runOnly = TRUE)

if (rlang::is_installed('ggplot2')) {
  test('plotClusterTrajectories', plotClusterTrajectories(model), runOnly = TRUE)
  test('plotTrajectories', plotTrajectories(model), runOnly = TRUE)
}

test('summary', is(summary(model), 'lcSummary'))
test('strip', is(strip(model), class(model)))

