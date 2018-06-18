library(cluslong)
options(deparse.max.lines=5)
data("testLongData")

clr1 = cluslongRecord(testLongData)
cluslong_kml(clr1, maxIter=NULL)
clr2 = cluslongRecord(testLongData)
cluslong(clr2, numClus=2:3, method='kml')

cluslong_gckm(clr1, gcmFixed=Value ~ poly(Time, 2), gcmRandom=Value ~ poly(Time, 2))

# GMM with gridsearch
clrGmm = cluslong_gmm(testLongData, fixed=Value ~ poly(Time, 2), mixture=~poly(Time, 2), random=~Time, startMaxIter=20, numRuns=5, numClus=3)
getResults(clrGmm, 3)@model$best

# classCov
clrGmmC = cluslong_gmm(testLongData, fixed=Value ~ poly(Time, 2), mixture=~poly(Time, 2), random=~1, numRuns=5, numClus=3, classCov=TRUE)
getResults(clrGmmC, 3)@model$best

# unstrucCov
clrGmmUS = cluslong_gmm(testLongData, fixed=Value ~ poly(Time, 2), mixture=~poly(Time, 2), random=~Time, numRuns=5, numClus=3, diagCov=FALSE)
getResults(clrGmmUS, 3)@model$best

# classCov+unstrucCov
clrGmmCUS = cluslong_gmm(testLongData, fixed=Value ~ poly(Time, 2), mixture=~poly(Time, 2), random=~Time, numRuns=5, numClus=3, classCov=TRUE, diagCov=FALSE)
getResults(clrGmmCUS, 3)@model$best


## GCKM + GMM
clrGmmTest = cluslong_gmm(testLongData, fixed=Value ~ poly(Time, 2), mixture=~poly(Time, 2), random=~Time, numClus=3, start='gckm')
getResults(clrGmmTest, 3)@model$best

# classCov
clrGmmTestC = cluslong_gmm(testLongData, fixed=Value ~ poly(Time, 2), mixture=~poly(Time, 2), random=~1, numClus=3, start='gckm', classCov=TRUE)
getResults(clrGmmTestC, 3)@model$best

# unstrucCov
clrGmmTestUS = cluslong_gmm(testLongData, fixed=Value ~ poly(Time, 2), mixture=~poly(Time, 2), random=~Time, numClus=3, start='gckm', diagCov=FALSE)
getResults(clrGmmTestUS, 3)@model$best

# classCov + unstrucCov
clrGmmTestCUS = cluslong_gmm(testLongData, fixed=Value ~ poly(Time, 2), mixture=~poly(Time, 2), random=~Time, numClus=3, start='gckm', classCov=TRUE, diagCov=FALSE)
getResults(clrGmmTestCUS, 3)@model$best
plotTrends(clrGmmTestCUS, 3)


clrGbtm = cluslong_gbtm(testLongData, fixed=Value ~ poly(Time, 2), mixture=~poly(Time, 2), startMaxIter=20, numRuns=5, numClus=3)
getResults(clrGbtm, 3)@model$best
clrGbtmTest = cluslong_gbtm(testLongData, fixed=Value ~ poly(Time, 2), mixture=~poly(Time, 2), startMaxIter=20, numRuns=5, numClus=3, start='kml')

cluslong_mixtvem(clr1, numRuns=5)

x = function(data) {
    cluslong(data, method='kml')
    return(data)
}
clrx = x(clr1)
clrx = do.call(x, list(clr1))

plotTrends(clr@results$c2)
plotTrends(clr, numClus=2)
plotTrends(clr, numClus=2, ribbon=TRUE)
plotTrends(clr, numClus=2, sample=1000)
plotTrajectories(clr, sample=100)

getCriterion(clr@results$c2, 'BIC')
BIC(clr@results$c2)

getCriterion(clr, 'BIC')
getCriterion(clr, c('BIC', 'AIC'))
getCriterion(clr)

plotCriterion(clr, 'BIC')
plotCriterion(clr, c('BIC', 'AIC'))
plotCriterion(clr, normalize=TRUE)
