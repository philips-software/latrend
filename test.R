library(cluslong)
data("testLongData")
data = testLongData
clr = cluslongRecord(data)
cluslong_kml(clr)
clr2 = cluslongRecord(data)
cluslong(clr2, method='kml')

x = function(data) {
    cluslong(data, method='kml')
    return(data)
}
invisible(x(clr1))
invisible(do.call(x, list(clr1)))

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
