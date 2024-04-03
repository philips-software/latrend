# KML ####
lcMethodTestKML = function(...) {
  lcMethodKML(response = 'Value', nbRedrawing = 1, maxIt = 10, ..., seed = 1)
}

# lcmm ####
lcMethodTestLcmmGMM = function(..., init = NULL, seed = 1) {
  mc = match.call.all()
  mc$fixed = Value ~ 1
  mc$maxiter = 10

  do.call(lcMethodLcmmGMM, as.list(mc)[-1])
}

lcMethodTestLcmmGBTM = function(..., init = NULL, seed = 1) {
  mc = match.call.all()
  mc$fixed = Value ~ 1
  mc$maxiter = 10

  do.call(lcMethodLcmmGBTM, as.list(mc)[-1])
}

# lme4 ####
lcMethodTestGCKM = function(...) {
  lcMethodGCKM(formula = Value ~ (1 | id), ...)
}


# Custom ####
lcMethodTestRandom = function(...) {
  lcMethodRandom(response = 'Value', ...)
}

lcMethodTestStratify = function(...) {
  lcMethodStratify(response = 'Value', ...)
}

lcMethodTestLMKM = function(...) {
  lcMethodLMKM(formula = Value ~ time, ..., seed = 1)
}


lcMethodTestTwoStep = function(...) {
  lcMethodFeature(response = 'Value', ...)
}


# Error ####
# method class that triggers an error during fitting
setClass('lcMethodError', contains = 'lcMethod')

lcMethodError = function(
  response = 'Value',
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  nClusters = 2,
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodError'
  do.call(new, as.list(mc))
}
