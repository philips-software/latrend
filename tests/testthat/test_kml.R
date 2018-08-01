context('KML')

test_that('KML', {
    clrA = cluslongRecord(testLongDataNamed)
    cluslong_kml(clrA, numClus=3:4, verbose=FALSE, seed=1)
    expect_length(clrA@results, 2)

    expect_equivalent(sort(getClusterProps(getResults(clrA, 3))), c(.242, .340, .418))
    expect_equivalent(sort(getClusterProps(getResults(clrA, 4))), c(.204, .214, .242, .340))

    # custom distance function
    clrB = cluslongRecord(testLongDataNamed)
    cluslong_kml(clrB, numClus=3, verbose=FALSE, seed=1, distance=function(x, y) sqrt(sum((x-y)^2)))
    expect_equivalent(sort(getClusterProps(getResults(clrA, 3))), sort(getClusterProps(getResults(clrB, 3))))
})