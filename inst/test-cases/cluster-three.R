# Tests for fitting three clusters with the lcMethod class
####

# Prepare data ####
refClusters = dataset[, first(Cluster), keyby = Id]$V1
S = uniqueN(dataset$Id)
trajNames = unique(dataset$Id)
clusSizes = dataset[, uniqueN(Id), keyby = Cluster]$V1
M = uniqueN(dataset$Time)

# Fit model ####
testData = copy(dataset) %>%
  setnames(c('Id', 'Time', 'Value'), c('Traj', 'Moment', 'Y'))

# Fit model ####
m = make.lcMethod(id = 'Traj', time = 'Moment', response = 'Y', nClusters = 3L)

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
test('nClusters', nClusters(model), 3L)
test('clusterNames', length(clusterNames(model)), 3L)
test('nIds', nIds(model), length(trajNames))
test('ids', ids(model), trajNames)
test('time', time(model), sort(unique(testData$Moment)))

test('postprob', postprob(model), runOnly = TRUE) # tests are done by postprob() generic
test('trajectoryAssignments.nIds', length(trajectoryAssignments(model)), nIds(model))
test(
  'trajectoryAssignments.uniqueN',
  uniqueN(trajectoryAssignments(model)),
  3L,
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

test('clusterSizes.len', length(clusterSizes(model)), 3)
test('clusterSizes.total', sum(clusterSizes(model)), S)
test(
  'clusterSizes.recovery',
  clusterSizes(model),
  clusSizes,
  check.attributes = FALSE,
  tolerance = 1,
  onFail = .Options$latrend.test.checkClusterRecovery
)
test('clusterProportions.len', length(clusterProportions(model)), 3)
test(
  'clusterProportions.recovery',
  clusterProportions(model),
  clusSizes / sum(clusSizes),
  check.attributes = FALSE,
  tolerance = .01,
  onFail = .Options$latrend.test.checkClusterRecovery
)

test('nobs', nobs(model), nrow(testData))
test('trajectories', trajectories(model), runOnly = TRUE)

test('clusterTrajectories', clusterTrajectories(model), runOnly = TRUE)

if (rlang::is_installed('ggplot2')) {
  test('plotClusterTrajectories', plotClusterTrajectories(model), runOnly = TRUE)
  test('plotTrajectories', plotTrajectories(model), runOnly = TRUE)
}
