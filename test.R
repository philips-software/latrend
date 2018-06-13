library(cluslong)
data("testLongData")
clr1 = cluslongRecord(testLongData)
cluslong_kml(clr1)
clr2 = cluslongRecord(testLongData)
cluslong(clr2, numClus=2:3, method='kml')

cluslong_gckm(clr1)
cluslong_gmm(clr1, fixed=Value ~ poly(Time, 2), mixture=~poly(Time, 2), random=~Time, numRuns=5)

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
