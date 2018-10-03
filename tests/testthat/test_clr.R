context('CluslongRecord')

test_that('data', {
    # Test keyed data.table
    data('testLongData')
    clr = cluslongRecord(testLongData)
    expect_s4_class(clr, 'CluslongRecord')
    expect_equal(clr@idCol, 'Subject')
    expect_equal(clr@timeCol, 'Assessment')
    expect_equal(clr@valueCol, 'Measurement')

    expect_equal(getIds(clr), as.character(unique(testLongData$Subject)))
    expect_equal(getTimes(clr), unique(testLongData$Assessment))

    expect_length(getResults(clr), 0)
    expect_s3_class(plotTrajectories(clr), 'ggplot')
})

test_that('clearing results', {
    clr = cluslongRecord(testLongData)
    cluslong_kml(data=clr, numClus=2:3, verbose=FALSE)
    expect_length(clr@results, 2)
    clearResults(clr)
    expect_length(clr@results, 0)
})

test_that('plots', {
    clr = cluslongRecord(testLongData)
    cluslong(clr, numClus=3:4, method='kml', verbose=FALSE)

    pCrit = plotCriterion(clr)
    expect_s3_class(pCrit, 'ggplot')
    suppressWarnings(print(pCrit))

    pCrit2 = plotCriterion(clr, normalize=TRUE)
    expect_s3_class(pCrit2, 'ggplot')
    suppressWarnings(print(pCrit2))

    pTraj = plotTrajectories(clr, ids=c('G1.1', 'G3.209'))
    expect_s3_class(pTraj, 'ggplot')
    print(pTraj)

    pTrend = plotTrends(clr, numClus=3)
    expect_s3_class(pTrend, 'ggplot')
    print(pTrend)

    pTrend2 = plotTrends(clr, numClus=3, ribbon=TRUE)
    expect_s3_class(pTrend2, 'ggplot')
    print(pTrend2)

    pCustom = plotCenters(clr, rep(1:2, each=250))
    expect_s3_class(pCustom, 'ggplot')
    print(pCustom)
})
