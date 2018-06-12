context('cluslong')

test_that('defaults', {
    clrA = cluslongRecord(testLongData)
    invisible(suppressMessages(capture.output(cluslong(clrA, numClus=1:2, method='kml'))))

    expect_length(clrA@results, 2)
    expect_equal(names(clrA@results), c('c1', 'c2'))

    clrB = cluslongRecord(testLongData)
    cluslong(clrB, numClus=3, method='kml', verbose=FALSE)

    expect_length(clrB@results, 1)
})

test_that('result merging', {
    clrA = cluslongRecord(testLongData)
    cluslong(clrA, numClus=2, method='kml', verbose=FALSE)
    expect_length(clrA@results, 1)
    cluslong(clrA, numClus=2:3, method='kml', verbose=FALSE)
    expect_length(clrA@results, 2)
    cluslong(clrA, numClus=4, method='kml', verbose=FALSE)
    expect_length(clrA@results, 3)
})

test_that('subfunction assignment', {
    x = function(clrIn) {
        cluslong(clrIn, numClus=2, method='kml', verbose=FALSE)
        return(clrIn)
    }

    clrTest = cluslongRecord(testLongData)
    clr = x(clrTest)

    expect_s4_class(clr, 'CluslongRecord')
    expect_length(clr@results, 1)
})

test_that('standard data.table input', {
    clrA = cluslong(testLongData, numClus=2, method='kml', verbose=FALSE)
    expect_equal(clrA@idCol, 'Id')
    expect_equal(clrA@timeCol, 'Time')
    expect_equal(clrA@valueCol, 'Value')
    expect_length(clrA@results, 1)
})

test_that('data.frame input', {
    clrA = cluslong(testLongDataFrame, numClus=2, method='kml', verbose=FALSE)
    expect_equal(clrA@idCol, 'PatientId')
    expect_equal(clrA@timeCol, 'Assessment')
    expect_equal(clrA@valueCol, 'Measurement')
    expect_length(clrA@results, 1)
})

test_that('method parameter passing', {
    clrA = cluslongRecord(testLongData)
    cluslong(clrA, numClus=2, center=median, method='kml', verbose=FALSE)
    expect_length(clrA@results, 1)
    expect_equivalent(clrA@results$c2@model['algorithm'], 'kmeans, slow (R)') # using median as the center results in kml using the R implementation
})