library(cluslong)
options(deparse.max.lines=3)
data("testLongData")

clr1 = cluslongRecord(testLongData)
cluslong_kml(clr1, maxIter=0)
clr2 = cluslongRecord(testLongData)
cluslong(clr2, numClus=2:3, method='kml')

cluslong_gckm(clr1, gcmFixed=Value ~ poly(Time, 2), gcmRandom=Value ~ poly(Time, 2))

clrGmm = cluslong_gmm(testLongData, fixed= ~ poly(Time, 2), mixture=~poly(Time, 2), random=~Time, startMaxIter=20, numRuns=5, numClus=3, catchError=TRUE)

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

plotCenters(clr1)

# missing data
library(data.table)
library(lcmm)
library(magrittr)
library(lme4)
library(emmeans)
library(ggplot2)
data("testLongData")
testLongData2 = copy(testLongData)[Id == 'G3.100' & Time %in% c(0, 1), Value := NA]
testLongData2[, Group := factor(substr(Id, 0, 2))]
testLongData2[Time == 0, mean(Value, na.rm=TRUE), by=Group]
testLongData2[Group == 'G1', TIC := .82] %>%
    .[Group == 'G2', TIC := .33] %>%
    .[Group == 'G3', TIC := .26]

testLongData2[1, TIC := NA]
mod = lmer(Value ~ 1 + TIC + (poly(Time, 2) | Id), data=testLongData2)
predict(mod, data.frame(Id='G1.1', Time=seq(0,1,by=.1), TIC=.75))

clrGmm = cluslong_gmm(testLongData2, fixed= Value ~ -1 + Time + I(Time^2) + TIC, mixture= ~ -1 + Time + I(Time^2), random=~Time, startMaxIter=50, numRuns=10, numClus=3)
plotTrends(clrGmm, 3)

summary(clrGmm@results$c3@model)
clrGmm@results$c3@model$pred
