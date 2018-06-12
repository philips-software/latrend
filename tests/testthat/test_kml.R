context('KML')

test_that('KML', {
    set.seed(1)
    clrA = cluslongRecord(testLongData)
    cluslong_kml(clrA, numClus=3:4, verbose=FALSE)
    expect_length(clrA@results, 2)

    expect_equivalent(sort(getClusterProps(getResults(clrA, 3))), c(.242, .340, .418))
    expect_equivalent(sort(getClusterProps(getResults(clrA, 4))), c(.204, .214, .242, .340))
})