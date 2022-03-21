# Tests for fitting a single cluster with the lcMethod class
####

# Prepare data ####
dataset1 = dataset[Cluster == Cluster[1]]
S = uniqueN(dataset1$Id)
trajNames = unique(dataset1$Id)
M = uniqueN(dataset1$Time)

# Fit model ####
testData = copy(dataset1) %>%
  setnames(c('Id', 'Time', 'Value'), c('Traj', 'Moment', 'Y'))

# Fit model ####
m = make.lcMethod(id = 'Traj', time = 'Moment', response = 'Y', nClusters = 1L)
model = latrend(m, testData)

# Tests ####
test('converged', !isFALSE(converged(model) > 0))
test('nClusters', nClusters(model), 1L)
test(
  'trajectoryAssignments',
  as.character(trajectoryAssignments(model)),
  rep(clusterNames(model)[1], S)
)
test('nIds', nIds(model), S)
test('nobs', nobs(model), nrow(testData))
test('ids', as.character(ids(model)), trajNames)
test('time', time(model), sort(unique(testData$Moment)))
test('postprob', postprob(model)[, 1], rep(1, S), check.attributes = FALSE)
test('clusterSizes', clusterSizes(model), S, check.attributes = FALSE)
test('clusterProportions', clusterProportions(model), 1, check.attributes = FALSE)
test('clusterTrajectories', clusterTrajectories(model), runOnly = TRUE)
