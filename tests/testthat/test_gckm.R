context('GCKM')

test_that('default', {
    clrA = cluslongRecord(testLongData)
    cluslong(clrA, numClus=2:3, method='gckm', verbose=FALSE)
    expect_length(clrA@results, 2)

    clr = cluslong_gckm(testLongData, numClus=2, verbose=FALSE)
    expect_length(clr@results, 1)

    clr = cluslong_gckm(testLongData, gcmRandom=Value ~ poly(Time, 2), numClus=2, verbose=FALSE)
    expect_length(clr@results, 1)
})