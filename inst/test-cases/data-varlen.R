# Tests for datasets comprising trajectories of different number of observations, but at aligned moments in time
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
ns = M - 0:2
testData = dataset2[, head(.SD, ns[.GRP %% length(ns) + 1L]), by = Id]
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
test('converged', !isFALSE(converged(model) > 0))
test('nClusters', nClusters(model), 2L)
test('nobs', nobs(model), nrow(testData))
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
