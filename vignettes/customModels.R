## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = TRUE
)

## ----setup, include = FALSE----------------------------------------------
library(ggplot2)
library(magrittr)
knitr::opts_chunk$set(
  cache = TRUE,
  collapse = TRUE,
  comment = "#>"
)
update_geom_defaults('line', list(size = .01))
update_geom_defaults('point', list(size = .5))

## ------------------------------------------------------------------------
library(cluslong)
options(cluslong.id = 'Traj', 
        cluslong.time = 'Time',
        cluslong.response = 'Y',
        cluslong.verbose = TRUE)

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
                        noiseScales = .05
                        )

## ------------------------------------------------------------------------
plotTrajectories(casedata)

## ------------------------------------------------------------------------
plotTrajectories(casedata) +
  geom_line(aes(y = Mu.fixed + Mu.cluster, color = Cluster), size = 2)

## ------------------------------------------------------------------------
ggplot(casedata[Time == 0], aes(x = Mu)) +
  geom_density(fill = 'gray', adjust = .3)

## ----message=TRUE--------------------------------------------------------
m = clMethodStratify(Y[1] > 1.7)
model = cluslong(m, casedata)

## ------------------------------------------------------------------------
clusterProportions(model)

## ----message=TRUE--------------------------------------------------------
stratfun = function(data) {
  int = coef(lm(Y ~ Time, data))[1]
  factor(int > 1.7, 
         levels = c(FALSE, TRUE), 
         labels = c('Low', 'High'))
}
m2 = clMethodStratify(stratfun, center = mean)
model2 = cluslong(m2, casedata)

clusterProportions(model2)

## ----message=TRUE--------------------------------------------------------
casedata[, Intercept := coef(lm(Y ~ Time, .SD))[1], by = Traj]

m3 = clMethodStratify(Intercept[1] > 1.7, clusterNames = c('Low', 'High'))
model3 = cluslong(m3, casedata)

## ------------------------------------------------------------------------
repStep = function(method, data, verbose) {
  coefdata = data[, lm(Y ~ Time, .SD) %>% coef() %>% as.list(), keyby = Traj]
  coefmat = subset(coefdata, select = -1) %>% as.matrix()
  rownames(coefmat) = coefdata$Traj
  return(coefmat)
}

## ------------------------------------------------------------------------
clusStep = function(method, data, repMat, envir, verbose) {
  km = kmeans(repMat, centers = 3)

  clModelCustom(method = method,
                data = data, 
                clusterAssignments = km$cluster,
                clusterTrajectories = method$center,
                model = km)
}

## ------------------------------------------------------------------------
m.twostep = clMethodTwoStep(representationStep = repStep, clusterStep = clusStep)

## ----message=TRUE--------------------------------------------------------
model.twostep = cluslong(m.twostep, data = casedata)
summary(model.twostep)

## ------------------------------------------------------------------------
repStep.gen = function(method, data, verbose) {
  coefdata = data[, lm(method$formula, .SD) %>% coef() %>% as.list(), keyby = c(method$id)]
  # exclude the id column
  coefmat = subset(coefdata, select = -1) %>% as.matrix()
  rownames(coefmat) = coefdata[[method$id]]
  return(coefmat)
}

clusStep.gen = function(method, data, repMat, envir, verbose) {
  km = kmeans(repMat, centers = method$nClusters)

  clModelCustom(method = method,
                data = data, 
                clusterAssignments = km$cluster,
                clusterTrajectories = method$center,
                model = km)
}

## ------------------------------------------------------------------------
m.twostepgen = clMethodTwoStep(representationStep = repStep.gen, 
                               clusterStep = clusStep.gen)

## ----message=TRUE--------------------------------------------------------
model.twostepgen = cluslong(m.twostepgen, formula = Y ~ Time, nClusters = 2, casedata)
summary(model.twostepgen)

## ------------------------------------------------------------------------
setClass('clMethodLMKM', contains='clMethod')

setMethod('getName', signature('clMethodLMKM'), function(object) 'lm-kmeans')

setMethod('getShortName', signature('clMethodLMKM'), function(object) 'lmkm')

## ------------------------------------------------------------------------
clMethodLMKM = function(formula=Value ~ Time,
                        time=getOption('cluslong.time'),
                        id=getOption('cluslong.id'),
                        nClusters=2) {
  new('clMethodLMKM', call=stackoverflow::match.call.defaults())
}

## ------------------------------------------------------------------------
setMethod('prepare', signature('clMethodLMKM'), function(method, data, verbose) {
  # optional data processing here
  return(NULL)
})

setMethod('fit', signature('clMethodLMKM'), function(method, data, envir, verbose, ...) {
  # fit lm per trajectory
  coefdata = data[, lm(method$formula, .SD) %>% coef() %>% as.list(), keyby = c(method$id)]
  # construct the coefficient matrix
  coefmat = subset(coefdata, select = -1) %>% as.matrix()
  # cross-sectional clustering
  km = kmeans(coefmat, centers = method$nClusters)
  
  new('clModelLMKM', 
      method=method,
      data=data,
      model=km,
      clusterNames=LETTERS[seq_len(method$nClusters)])
})

## ------------------------------------------------------------------------
setClass('clModelLcmmGMM', contains='clModel')

