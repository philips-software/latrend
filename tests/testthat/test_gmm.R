context('GMM')

test_that('GMM', {
    clrA = cluslongRecord(testLongDataNamed[Assessment %in% (c(0, 5, 9)/9)])
    cluslong_gmm(clrA, numClus=3,
                 fixed=Measurement~Assessment,
                 mixture=~Assessment,
                 random=~Assessment, start='gridsearch', numRuns=5, startMaxIter=20,
                 verbose=FALSE, seed=1)
    expect_length(clrA@results, 1)

    expect_equivalent(sort(getClusterProps(getResults(clrA, 3))), c(.244, .338, .418))
})
