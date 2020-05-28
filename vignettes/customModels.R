## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE----------------------------------------------
library(cluslong)
library(ggplot2)
knitr::opts_chunk$set(
  cache = TRUE,
  collapse = TRUE,
  comment = "#>"
)
update_geom_defaults('line', list(size = .01))
update_geom_defaults('point', list(size = .5))
options(cluslong.verbose=TRUE)

## ------------------------------------------------------------------------
options(cluslong.id = 'Traj', 
        cluslong.time = 'Time',
        cluslong.response = 'Y')

## ------------------------------------------------------------------------
set.seed(1)
casedata = generateLongData(sizes = c(40, 60), 
                        data = data.frame(Time = 0:10),
                        fixed = Y ~ 1,
                        fixedCoefs = 1,
                        cluster = ~ Time, 
                        clusterCoefs = cbind(c(1, -.05), c(0, .05)),
                        random = ~ Time,
                        randomScales = cbind(c(.3, .1), c(.3, .05)),
                        noiseScales=.05
                        )

## ------------------------------------------------------------------------
plotTrajectories(casedata)

## ------------------------------------------------------------------------
plotTrajectories(casedata) +
  geom_line(aes(y=Mu.fixed + Mu.cluster, color=Cluster), size=2)

## ------------------------------------------------------------------------
ggplot(casedata[Time == 0], aes(x=Mu)) +
  geom_density(fill='gray', adjust=.3)

## ------------------------------------------------------------------------
m = clMethodStratify(Y[1] > 1.7)
model = cluslong(m, casedata)

## ------------------------------------------------------------------------
clusterProportions(model)

## ------------------------------------------------------------------------
stratfun = function(data) {
  int = coef(lm(Y ~ Time, data))[1]
  factor(int > 1.7, levels=c(FALSE, TRUE), labels=c('Low', 'High'))
}
m2 = clMethodStratify(stratfun, center=mean)
model2 = cluslong(m2, casedata)

clusterProportions(model2)

## ------------------------------------------------------------------------
casedata[, Intercept := coef(lm(Y ~ Time, .SD))[1], by=Traj]

m3 = clMethodStratify(Intercept[1] > 1.7, clusterNames=c('Low', 'High'))
model3 = cluslong(m3, casedata)

## ------------------------------------------------------------------------
repStep = function(method, data) {
  coefdata = data[, coef(lm(method$formula, .SD)), keyby=c(method$id)]
  # exclude the id column
  coefmat = subset(coefdata, select=-1) %>% as.matrix()
  rownames(coefmat) = coefdata[[method$id]]
  return(coefmat)
}

## ------------------------------------------------------------------------
clusStep = function() {
  
}

## ------------------------------------------------------------------------
mtwo = clMethodTwoStep(repStep, clusStep)

