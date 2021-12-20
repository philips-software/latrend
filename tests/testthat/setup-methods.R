# test method with deterministic result
mTest = lcMethodTestLMKM(nClusters = 2)
# test method with random result (no seed argument)
mTestRandom = lcMethodKML(response = 'Value', nbRedrawing = 1, maxIt = 10)

mRandom = lcMethodTestRandom()
mError = lcMethodError()
