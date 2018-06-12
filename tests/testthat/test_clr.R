context('CluslongRecord')

test_that('data with standard names', {
    # Test keyed data.table
    clr = cluslongRecord(testLongData)
    expect_s4_class(clr, 'CluslongRecord')
    expect_equal(clr@idCol, 'Id')
    expect_equal(clr@timeCol, 'Time')
    expect_equal(clr@valueCol, 'Value')

    expect_equal(getIds(clr), as.character(unique(testLongData$Id)))
    expect_equal(getTimes(clr), unique(testLongData$Time))

    expect_length(getResults(clr), 0)
    expect_s3_class(plotTrajectories(clr), 'ggplot')
})

test_that('data with custom names', {
    # Test data.frame
    clrFrame = cluslongRecord(testLongDataFrame)
    expect_s4_class(clrFrame, 'CluslongRecord')
    expect_equal(clrFrame@idCol, 'PatientId')
    expect_equal(clrFrame@timeCol, 'Assessment')
    expect_equal(clrFrame@valueCol, 'Measurement')

    # Test keyed data.table
    clrNamed = cluslongRecord(testLongDataNamed)
    expect_s4_class(clrNamed, 'CluslongRecord')
    expect_equal(clrNamed@idCol, 'PatientId')
    expect_equal(clrNamed@timeCol, 'Assessment')
    expect_equal(clrNamed@valueCol, 'Measurement')

    expect_s3_class(plotTrajectories(clrNamed), 'ggplot')
})

test_that('clearing results', {
    clrA = cluslongRecord(testLongData)
    cluslong(clrA, numClus=2:3, method='kml', verbose=FALSE)
    expect_length(clrA@results, 2)
    clearResults(clrA)
    expect_length(clrA@results, 0)
})

test_that('plots', {
    clrA = cluslongRecord(testLongData)
    cluslong(clrA, numClus=3, method='kml', verbose=FALSE)

    pCrit = plotCriterion(clrA)
    expect_s3_class(pCrit, 'ggplot')
    suppressWarnings(print(pCrit))

    pTrend = plotTrends(clrA, numClus=3)
    expect_s3_class(pTrend, 'ggplot')
    print(pTrend)
})

test_that('plots with custom names', {
    clrA = cluslongRecord(testLongDataNamed)
    cluslong(clrA, numClus=3, method='kml', verbose=FALSE)

    pCrit = plotCriterion(clrA, c('BIC', 'AIC'))
    expect_s3_class(pCrit, 'ggplot')
    suppressWarnings(print(pCrit))

    pCrit2 = plotCriterion(clrA, normalize=TRUE)
    expect_s3_class(pCrit2, 'ggplot')
    suppressWarnings(print(pCrit2))

    pTrend = plotTrends(clrA, numClus=3, sample=100)
    expect_s3_class(pTrend, 'ggplot')
    print(pTrend)

    pTrend2 = plotTrends(clrA, numClus=3, ribbon=TRUE)
    expect_s3_class(pTrend2, 'ggplot')
    print(pTrend2)
})