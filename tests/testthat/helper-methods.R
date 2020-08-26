lcMethodTestKML = function(...) {
  lcMethodKML(response = 'Value', nbRedrawing=1, maxIt=10, ..., seed=1)
}

lcMethodTestLcmmGMM = function(...) {
  lcMethodLcmmGMM(formula = Value ~ 1 + CLUSTER + (1 | ID), maxiter=10, ..., seed=1)
}

lcMethodTestLcmmGBTM = function(...) {
  lcMethodLcmmGBTM(formula = Value ~ CLUSTER, maxiter=10, ..., seed=1)
}

lcMethodTestFlexmixGBTM = function(...) {
  lcMethodFlexmixGBTM(formula = Value ~ Assessment, ..., control=list(iter.max=1, tolerance=1e-3), seed=1)
}

lcMethodTestCrimCV = function(...) {
  lcMethodCrimCV(response = 'Value', ..., model='ZIP', dpolyp=2, dpolyl=1, init=5, seed=1)
}

lcMethodTestFunFEM = function(...) {
  lcMethodFunFEM(response = 'Value', ...)
}

lcMethodTestMclustLLPA = function(...) {
  lcMethodMclustLLPA(response = 'Value', ...)
}

lcMethodTestRandom = function(...) {
  lcMethodRandom(response = 'Value', ...)
}

lcMethodTestStratify = function(...) {
  lcMethodStratify(response = 'Value', ...)
}

lcMethodTestFlexmix = function(...) {
  lcMethodFlexmix(formula = Value ~ 0, ...)
}

lcMethodTestGCKM = function(...) {
  lcMethodGCKM(formula = Value ~ Assessment, ...)
}

lcMethodTestLMKM = function(...) {
  lcMethodLMKM(formula = Value ~ Assessment, ...)
}

lcMethodTestTwoStep = function(...) {
  lcMethodTwoStep(response = 'Value', ...)
}

lcMethodTestMixTVEM = function(...) {
  lcMethodMixTVEM(formula = Value ~ time(1) - 1, ...)
}

lcMethodTestCrimCVt = function(...) {
  lcMethodCrimCV(response = 'Value', ..., model='ZIPt', dpolyp=2, init=5, seed=1)
}

lcMethodTestLongclust = function(...) {
  lcMethodLongclust(response = 'Value', modelSubset='VVA', gaussian=TRUE, ..., seed=1)
}

lcMethodTestLongclustT = function(...) {
  lcMethodLongclust(response = 'Value', modelSubset='VEI', gaussian=FALSE, ..., seed=1)
}

lcMethodTestMixtoolsNPRM = function(...) {
  lcMethodMixtoolsNPRM(response = 'Value', maxiter=10, eps=1e-04, seed=1)
}

lcMethodTestMixtoolsGMM = function(...) {
  lcMethodMixtoolsGMM(formula = Value ~ Assessment + (Assessment | Traj), epsilon=1e-02, ..., seed=1)
}

lcMethodTestMixAK_GLMM = function(...) {
  lcMethodMixAK_GLMM(fixed = Value ~ 1, random = ~ Assessment, ..., seed=1)
}
