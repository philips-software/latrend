context('CluslongResult')

test_that('functions', {
    set.seed(1)
    clr = cluslongRecord(testLongData)
    cluslong(clr, numClus=3, method='kml', verbose=FALSE)

    clResult = getResults(clr, 3)
    expect_equal(getClusterNames(clResult), LETTERS[1:3])
    expect_length(getClusterProps(clResult), 3)
    expect_named(getClusterProps(clResult))
    expect(sum(getClusterProps(clResult)) == 1)

    expect_equivalent(round(BIC(clResult)), -5213)
})

test_that('plots', {
    clr = cluslongRecord(testLongData)
    cluslong(clr, numClus=3, method='kml', verbose=FALSE)

    clResult = getResults(clr, 3)

    pTrend = plotTrends(clResult)
    expect_s3_class(pTrend, 'ggplot')
    print(pTrend)
})
