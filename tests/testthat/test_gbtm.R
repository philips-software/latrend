context('GBTM')

test_that('GBTM', {
    clrA = cluslongRecord(testLongDataNamed[Assessment %in% (c(0, 5, 9)/9)])
    cluslong_gbtm(clrA, numClus=3,
                 fixed=Measurement~Assessment,
                 mixture=~Assessment, start='gridsearch', numRuns=5, startMaxIter=20,
                 verbose=FALSE, seed=1)
    expect_length(clrA@results, 1)

    expect_equivalent(sort(getClusterProps(getResults(clrA, 3))), c(.244, .338, .418))
})

test_that('init', {
    clrA = cluslong_gbtm(testLongDataNamed,
                         fixed=Measurement ~ poly(Assessment, 2),
                         mixture=~poly(Assessment, 2),
                         numClus=3, start='kml', verbose=FALSE, seed=1)
    expect_equivalent(sort(getClusterProps(getResults(clrA, 3))), c(.24, .340, .42), tolerance=.01)
})
