# Tests for datasets comprising missing or NA observations or trajectories
####

# Prepare data ####
dataset2 = dataset[Cluster %in% unique(Cluster)[1:2]]
refClusters = dataset2[, first(Cluster), keyby = Id]$V1
S = uniqueN(dataset2$Id)
trajNames = unique(dataset2$Id)
moments = unique(dataset2$Time)
clusSizes = dataset2[, uniqueN(Id), keyby = Cluster]$V1
M = uniqueN(dataset2$Time)

# remove an observation from the first trajectory
testData = subset(dataset2, select = c('Id', 'Time', 'Value'))
testData[2, 'Value'] = NA
m = make.lcMethod(id = 'Id', time = 'Time', response = 'Value', nClusters = 2L)
model = latrend(m, data = testData)

refModel = lcModelPartition(
  testData,
  response = 'Value',
  time = 'Time',
  id = 'Id',
  trajectoryAssignments = refClusters
)

# Tests ####
test('oneTraj.converged', !isFALSE(converged(model) > 0))
test('oneTraj.nClusters', nClusters(model), 2L)
test('oneTraj.nobs', nobs(model), nrow(testData) - 1L)
test(
  'trajectoryAssignments.uniqueN',
  uniqueN(trajectoryAssignments(model)),
  2L,
  text = 'expecting each cluster to have at least some assigned trajectories'
)
test(
  'oneTraj.trajectoryAssignments.recovery',
  externalMetric(model, refModel, 'adjustedRand') > .99,
  check.attributes = FALSE,
  text = 'does trajectory assignment match the reference?',
  onFail = .Options$latrend.test.checkClusterRecovery
)

# remove an observation from each trajectory
testData2 = subset(dataset2, select = c('Id', 'Time', 'Value'))
testData2[, Value := replace(Value, .GRP %% M + 1L, NA), by = Id]

m2 = make.lcMethod(id = 'Id', time = 'Time', response = 'Value', nClusters = 2L)
model2 = latrend(m2, data = testData)

# Tests ####
test('eachTraj.converged', !isFALSE(converged(model2) > 0))
test('eachTraj.nClusters', nClusters(model2), 2L)
test('eachTraj.nobs', nobs(model2), nrow(testData) - 1L)
test(
  'eachTraj.trajectoryAssignments.uniqueN',
  uniqueN(trajectoryAssignments(model2)),
  2L,
  text = 'expecting each cluster to have at least some assigned trajectories'
)
test(
  'eachTraj.trajectoryAssignments.recovery',
  externalMetric(model2, refModel, 'adjustedRand') > .99,
  check.attributes = FALSE,
  text = 'does trajectory assignment match the reference?',
  onFail = .Options$latrend.test.checkClusterRecovery
)
