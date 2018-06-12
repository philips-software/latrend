context('GCKM')

test_that('default', {
    clrA = cluslongRecord(testLongData)
    cluslong(clrA, numClus=2:3, method='gckm', verbose=FALSE,
             gcmFixed=Value ~ Time,
             gcmRandom=Value ~ Time)
    expect_length(clrA@results, 2)

    clr = cluslong_gckm(testLongData, numClus=2, verbose=FALSE,
                        gcmFixed=Value ~ Time,
                        gcmRandom=Value ~ Time)
    expect_length(clr@results, 1)

    clr = cluslong_gckm(testLongData, numClus=2, verbose=FALSE,
                        gcmFixed=Value ~ Time,
                        gcmRandom=Value ~ poly(Time, 2))
    expect_length(clr@results, 1)
})

test_that('named input', {
    clr = cluslong_gckm(testLongDataNamed, numClus=2, verbose=FALSE,
                        gcmFixed =Measurement ~ Assessment,
                        gcmRandom=Measurement ~ Assessment)
    expect_length(clr@results, 1)
})

test_that('results', {
    clrA = cluslongRecord(testLongData)
    cluslong(clrA, numClus=2:3, method='gckm', verbose=FALSE, seed=1,
             gcmFixed=Value ~ Time,
             gcmRandom=Value ~ Time)
    expect_equivalent(sort(getClusterProps(clrA@results$c3)), c(.240, .342, .418))
})