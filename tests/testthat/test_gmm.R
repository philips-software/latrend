context('GMM')

test_that('GMM', {
    clr = cluslongRecord(testLongData[Assessment %in% (c(0, 5, 9)/9)])
    cluslong_gmm(clr, numClus=3,
                 fixed=Measurement~Assessment,
                 mixture=~Assessment,
                 random=~Assessment, start='gridsearch', numRuns=5, startMaxIter=20,
                 verbose=FALSE, seed=1)
    expect_length(clr@results, 1)

    expect_equivalent(sort(getClusterProps(getResults(clr, 3))), c(.244, .338, .418))
})

test_that('init', {
    clr = cluslong_gmm(testLongData,
                         fixed=Measurement ~ poly(Assessment, 2),
                         mixture=~poly(Assessment, 2),
                         random=~Assessment,
                         numClus=3, start='gckm', verbose=FALSE, seed=1)
    expect_equivalent(sort(getClusterProps(getResults(clr, 3))), c(.24, .340, .42), tolerance=.01)
})
