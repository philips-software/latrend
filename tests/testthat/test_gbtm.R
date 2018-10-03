context('GBTM')

test_that('GBTM', {
    clr = cluslongRecord(testLongData[Assessment %in% (c(0, 5, 9)/9)])
    cluslong_gbtm(clr, numClus=3,
                 fixed=Measurement~Assessment,
                 mixture=~Assessment, start='gridsearch', numRuns=5, startMaxIter=20,
                 verbose=FALSE, seed=1)
    expect_length(clr@results, 1)

    expect_equivalent(sort(getClusterProps(getResults(clr, 3))), c(.244, .338, .418))
})

test_that('init', {
    clr = cluslong_gbtm(testLongData,
                         fixed=Measurement ~ poly(Assessment, 2),
                         mixture=~poly(Assessment, 2),
                         numClus=3, start='kml', verbose=FALSE, seed=1)
    expect_equivalent(sort(getClusterProps(getResults(clr, 3))), c(.24, .340, .42), tolerance=.01)
})
