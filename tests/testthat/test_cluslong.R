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
    clr = cluslongRecord(testLongData)
    cluslong(clr, numClus=2, method='kml', verbose=FALSE)
    expect_length(clr@results, 1)
    cluslong(clr, numClus=2:3, method='kml', verbose=FALSE)
    expect_length(clr@results, 2)
    cluslong(clr, numClus=4, method='kml', verbose=FALSE)
    expect_length(clr@results, 3)
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
    clr = cluslong(testLongData, numClus=2, method='kml', verbose=FALSE)
    expect_equal(clr@idCol, 'Subject')
    expect_equal(clr@timeCol, 'Assessment')
    expect_equal(clr@valueCol, 'Measurement')
    expect_length(clr@results, 1)
})

test_that('data.frame input', {
    clr = cluslong(as.data.frame(testLongData), numClus=2, method='kml', verbose=FALSE)
    expect_equal(clr@idCol, 'Subject')
    expect_equal(clr@timeCol, 'Assessment')
    expect_equal(clr@valueCol, 'Measurement')
    expect_length(clr@results, 1)
})

test_that('method parameter passing', {
    clr = cluslongRecord(testLongData)
    cluslong(clr, numClus=2, center=median, method='kml', verbose=FALSE)
    expect_length(clr@results, 1)
    expect_equivalent(clr@results$c2@model['algorithm'], 'kmeans, slow (R)') # using median as the center results in kml using the R implementation
})

test_that('resultfun', {
    clrA = cluslongRecord(testLongData)
    cluslong(clrA, numClus=2, method='kml', verbose=FALSE, result=function(clr, clResult) {
        stopifnot(is.CluslongRecord(clr))
        stopifnot(is.CluslongResult(clResult))
        clResult@details['test'] = 25
        return(clResult)
    })
    expect_equal(getResults(clrA, 2)@details$test, 25)

    # result via options()
    clrB = cluslongRecord(testLongData)
    options(cluslong.result=function(clr, clResult) { clResult@details['test2'] = 15; return(clResult)})
    cluslong(clrB, numClus=2, method='kml', verbose=FALSE)
    options(cluslong.result=NULL)
    expect_equal(getResults(clrB, 2)@details$test2, 15)
})