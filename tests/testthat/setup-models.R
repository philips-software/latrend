modelTest1 = latrend(mTest, data = testLongData, nClusters = 1)
modelTest2 = modelTest = latrend(mTest, data = testLongData)
modelTest3 = latrend(mTest, data = testLongData, nClusters = 3)

modelTestRandom = latrend(mTestRandom, data = testLongData)

kml1 = latrend(lcMethodTestKML(), data = testLongData, nClusters = 1)
kml2 = latrend(lcMethodTestKML(), data = testLongData, nClusters = 2)
kml3 = latrend(lcMethodTestKML(), data = testLongData, nClusters = 3)
kml4 = latrend(lcMethodTestKML(), data = testLongData, nClusters = 4)

gmm1 = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters = 1)
gmm2 = gmm = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters = 2)
gmm3 = latrend(lcMethodTestLcmmGMM(), testLongData, nClusters = 3)
