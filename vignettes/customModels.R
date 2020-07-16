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
library(latrend)
options(latrend.id = 'Traj',
        latrend.time = 'Time',
        latrend.verbose = TRUE)

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
plotTrajectories(casedata, response = 'Y')

## ------------------------------------------------------------------------
plotTrajectories(casedata, response = 'Y') +
  geom_line(aes(y = Mu.fixed + Mu.cluster, color = Cluster), size = 2)

## ------------------------------------------------------------------------
ggplot(casedata[Time == 0], aes(x = Mu)) +
  geom_density(fill = 'gray', adjust = .3)

## ----message=TRUE--------------------------------------------------------
m = lcMethodStratify(response = 'Y', Y[1] > 1.7)
model = latrend(m, casedata)

## ------------------------------------------------------------------------
clusterProportions(model)

## ----message=TRUE--------------------------------------------------------
stratfun = function(data) {
  int = coef(lm(Y ~ Time, data))[1]
  factor(int > 1.7,
         levels = c(FALSE, TRUE),
         labels = c('Low', 'High'))
}
m2 = lcMethodStratify(response = 'Y', stratify = stratfun, center = mean)
model2 = latrend(m2, casedata)

clusterProportions(model2)

## ----message=TRUE--------------------------------------------------------
casedata[, Intercept := coef(lm(Y ~ Time, .SD))[1], by = Traj]

m3 = lcMethodStratify(response = 'Y', Intercept[1] > 1.7, clusterNames = c('Low', 'High'))
model3 = latrend(m3, casedata)

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

  lcModelCustom(method = method,
                data = data,
                clusterAssignments = km$cluster,
                clusterTrajectories = method$center,
                model = km)
}

## ------------------------------------------------------------------------
m.twostep = lcMethodTwoStep(response = 'Y',
                            representationStep = repStep,
                            clusterStep = clusStep)

## ----message=TRUE--------------------------------------------------------
model.twostep = latrend(m.twostep, data = casedata)
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

  lcModelCustom(method = method,
                response = method$response,
                data = data,
                clusterAssignments = km$cluster,
                clusterTrajectories = method$center,
                model = km)
}

## ------------------------------------------------------------------------
m.twostepgen = lcMethodTwoStep(response = 'Y',
                               representationStep = repStep.gen,
                               clusterStep = clusStep.gen)

## ----message=TRUE--------------------------------------------------------
model.twostepgen = latrend(m.twostepgen, formula = Y ~ Time, nClusters = 2, casedata)
summary(model.twostepgen)

## ------------------------------------------------------------------------
setClass('lcMethodLMKM', contains='lcMethod')

setMethod('getName', signature('lcMethodLMKM'), function(object) 'lm-kmeans')

setMethod('getShortName', signature('lcMethodLMKM'), function(object) 'lmkm')

## ------------------------------------------------------------------------
lcMethodLMKM = function(formula,
                        time = getOption('latrend.time'),
                        id = getOption('latrend.id'),
                        nClusters = 2) {
  new('lcMethodLMKM', call = stackoverflow::match.call.defaults())
}

## ------------------------------------------------------------------------
setMethod('prepareData', signature('lcMethodLMKM'), function(method, data, verbose) {
  # optional data processing here
  return(NULL)
})

setMethod('fit', signature('lcMethodLMKM'), function(method, data, envir, verbose, ...) {
  # fit lm per trajectory
  coefdata = data[, lm(method$formula, .SD) %>% coef() %>% as.list(), keyby = c(method$id)]
  # construct the coefficient matrix
  coefmat = subset(coefdata, select = -1) %>% as.matrix()
  # cross-sectional clustering
  km = kmeans(coefmat, centers = method$nClusters)

  new('lcModelLMKM',
      method=method,
      data=data,
      model=km,
      clusterNames=LETTERS[seq_len(method$nClusters)])
})

## ------------------------------------------------------------------------
setClass('lcModelLcmmGMM', contains='lcModel')

