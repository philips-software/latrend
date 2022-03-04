# Tests for fitting three clusters with the lcMethod class
####

# Prepare data ####
S = 25
M = 4
trajNames = paste0('S', seq_len(S * 3L))
moments = seq_len(M)

testData = data.table(
  Traj = rep(trajNames, each = M) %>% factor(levels = trajNames),
  Moment = rep(moments, S * 3L),
  Y = c(
    rnorm(S * M, mean = -7),
    rnorm(S * M, mean = 0),
    rnorm(S * M, mean = 7)
  ),
  Class = rep(LETTERS[1:3], each = S * M)
)
refClusters = rep(LETTERS[1:3], each = S)

# Fit model ####
m = make.lcMethod(id = 'Traj', time = 'Moment', response = 'Y', nClusters = 3)

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
test('nClusters', nClusters(model), 3L)
test('clusterNames', length(clusterNames(model)), 3L)
test('nIds', nIds(model), length(trajNames))
test('ids', ids(model), trajNames)
test('time', time(model), sort(unique(testData$Moment)))

test('postprob', postprob(model), runOnly = TRUE) # tests are done by postprob() generic
test('trajectoryAssignments.nIds', length(trajectoryAssignments(model)), nIds(model))
test(
  'trajectoryAssignments.ref',
  externalMetric(model, refModel, 'adjustedRand') == 1,
  check.attributes = FALSE,
  text = 'does trajectory assignment match the reference?'
)
test('clusterSizes', clusterSizes(model), c(S, S, S), check.attributes = FALSE)
test('clusterProportions', clusterProportions(model), c(1/3, 1/3, 1/3), check.attributes = FALSE)

test('nobs', nobs(model), nrow(testData))
test('trajectories', trajectories(model), runOnly = TRUE)

test('clusterTrajectories', clusterTrajectories(model), runOnly = TRUE)

test('plotClusterTrajectories', plotClusterTrajectories(model), runOnly = TRUE)
test('plotTrajectories', plotTrajectories(model), runOnly = TRUE)
