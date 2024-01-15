test_that('make.clusterSizeLabels', {
  expect_error(
    make.clusterSizeLabels(character(), numeric())
  )
  expect_error(
    make.clusterSizeLabels('A', numeric())
  )
  expect_error(
    make.clusterSizeLabels('A', -1)
  )
  expect_error(
    make.clusterSizeLabels('A', NA)
  )
  expect_error(
    make.clusterSizeLabels('A', c(1, 2))
  )
  expect_error(
    make.clusterSizeLabels(c('A', 'B'), 1)
  )

  expect_equal(
    make.clusterSizeLabels('A', 1),
    'A (1)'
  )

  expect_equal(
    make.clusterSizeLabels('A', 0),
    'A (0)'
  )

  expect_equal(
    make.clusterSizeLabels(c('A', 'B'), c(0, 10)),
    c('A (0)', 'B (10)')
  )

  expect_equal(
    make.clusterSizeLabels(c('B', 'A'), c(50, 10)),
    c('B (50)', 'A (10)')
  )

  expect_equal(
    make.clusterSizeLabels(c('Abc', 'Def', 'Ghi'), c(50, 10, 1e3)),
    c('Abc (50)', 'Def (10)', 'Ghi (1000)')
  )
})


test_that('make.clusterPropLabels', {
  expect_equal(
    make.clusterPropLabels('A', 1),
    'A (100%)'
  )

  expect_equal(
    make.clusterPropLabels(c('B', 'A'), c(30, 20)),
    c('B (60%)', 'A (40%)')
  )

  expect_equal(
    make.clusterPropLabels(c('B', 'A'), c(40, 20)),
    c('B (67%)', 'A (33%)')
  )

  expect_equal(
    make.clusterPropLabels(c('B', 'E', 'A'), c(30, 0, 20)),
    c('B (60%)', 'E (0%)', 'A (40%)')
  )
})


test_that('make.orderedClusterNames', {
  expect_equal(
    make.orderedClusterNames('A', 1),
    'A'
  )

  expect_equal(
    make.orderedClusterNames('A', 'A'),
    'A'
  )

  expect_equal(
    make.orderedClusterNames(c('A', 'B'), 2),
    'B'
  )

  expect_equal(
    make.orderedClusterNames(c('A', 'B'), 'B'),
    'B'
  )

  expect_equal(
    make.orderedClusterNames(c('A', 'B'), 2:1),
    c('B', 'A')
  )

  expect_equal(
    make.orderedClusterNames(c('A', 'B'), c('B', 'A')),
    c('B', 'A')
  )

  expect_equal(
    make.orderedClusterNames(c('A', 'B', 'C'), c(1, 3)),
    c('A', 'C')
  )

  expect_equal(
    make.orderedClusterNames(c('A', 'B', 'C'), c('A', 'C')),
    c('A', 'C')
  )

  expect_equal(
    make.orderedClusterNames(c('A', 'B', 'C'), numeric()),
    c('A', 'B', 'C')
  )

  expect_error({
    make.orderedClusterNames(c('A', 'B', 'C'), c(0, 1))
  })

  expect_error({
    make.orderedClusterNames(c('A', 'B', 'C'), c('A', 'D'))
  })

  expect_error({
    make.orderedClusterNames(c('A', 'B', 'C'), c('A', 'A', 'B'))
  })
})
