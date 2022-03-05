# Tests for the basic process from fitting an easy dataset via the lcMethod class,
# and checking whether the resulting lcModel is valid
####

# Prepare data ####
S = 25 # trajectories per cluster
M = 4 # number of observations per trajectory
trajNames = paste0('S', seq_len(S * 2))
moments = seq_len(M)

testData = data.table(
  Traj = rep(trajNames, each = M) %>% factor(levels = trajNames),
  Moment = rep(moments, S * 2),
  Y = c(
    rnorm(S * M, mean = -5),
    rnorm(S * M, mean = 5)
  ),
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
test('getLcMethod', is(getLcMethod(model), class(m)))
test('getName.nzchar', nzchar(getName(model)))
test('getShortName.nzchar', nzchar(getShortName(model)))
test('idVariable', idVariable(model), 'Traj')
test('timeVariable', timeVariable(model), 'Moment')
test('responseVariable', responseVariable(model), 'Y')
test('getCall', is(getCall(model), 'call'))
test('converged', converged(model) > 0)
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
test(
  'trajectoryAssignments.recovery',
  externalMetric(model, refModel, 'adjustedRand') == 1,
  check.attributes = FALSE,
  text = 'does trajectory assignment match the reference?',
  onFail = .Options$latrend.test.checkClusterRecovery
)
test('clusterSizes.len', length(clusterSizes(model)), 2)
test('clusterSizes.total', sum(clusterSizes(model)), S * 2)
test(
  'clusterSizes.recovery',
  clusterSizes(model),
  c(S, S),
  check.attributes = FALSE,
  onFail = .Options$latrend.test.checkClusterRecovery
)
test('clusterProportions.len', length(clusterProportions(model)), 2)
test(
  'clusterProportions.recovery',
  clusterProportions(model),
  c(.5, .5),
  check.attributes = FALSE,
  onFail = .Options$latrend.test.checkClusterRecovery
)

test('model.data', is.data.frame(model.data(model)))
test('model.data.nrow', nrow(model.data(model)) == nrow(testData))
test('nobs', nobs(model), nrow(testData))
test('trajectories', trajectories(model), runOnly = TRUE)

test('clusterTrajectories', clusterTrajectories(model), runOnly = TRUE)
test('plotClusterTrajectories', plotClusterTrajectories(model), runOnly = TRUE)
test('plotTrajectories', plotTrajectories(model), runOnly = TRUE)

test('summary', is(summary(model), 'lcSummary'))
test('strip', is(strip(model), class(model)))
