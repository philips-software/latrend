# Tests for fitting a single cluster with the lcMethod class
####

# Prepare data ####
S = 25
M = 4
trajNames = paste0('S', seq_len(S))
moments = seq_len(M)
testData1 = data.table(
  Traj = rep(trajNames, each = M) %>% factor(levels = trajNames),
  Moment = rep(moments, S),
  Y = rnorm(S * M)
)

# Fit model ####
m1 = make.lcMethod(id = 'Traj', time = 'Moment', response = 'Y')
model = latrend(m1, testData1)

# Tests ####
test('converged', !isFALSE(converged(model) > 0))
test('nClusters', nClusters(model), 1L)
test(
  'trajectoryAssignments',
  as.character(trajectoryAssignments(model)),
  rep(clusterNames(model)[1], S)
)
test('nIds', nIds(model), S)
test('nobs', nobs(model), nrow(testData1))
test('ids', as.character(ids(model)), trajNames)
test('time', time(model), sort(unique(testData1$Moment)))
test('postprob', postprob(model)[, 1], rep(1, S), check.attributes = FALSE)
test('clusterSizes', clusterSizes(model), S, check.attributes = FALSE)
test('clusterProportions', clusterProportions(model), 1, check.attributes = FALSE)
test('clusterTrajectories', clusterTrajectories(model), runOnly = TRUE)
