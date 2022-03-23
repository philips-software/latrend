context('assert')
rngReset()

m1 = modelTest
m2 = modelTestRandom

test_that('is_named', {
  expect_true(is_named(setNames(1:2, LETTERS[1:2])))
  expect_true(is_named(setNames(character(), character()))) #empty named char vector

  expect_false(is_named(NULL))
  expect_false(is_named(character()))
  expect_false(is_named(character(2)))
  expect_false(is_named(NA))
  expect_false(is_named(1:2))
  expect_false(is_named(factor(1:2, levels = LETTERS[1:2])))
})

test_that('is_data', {
  expect_false(is_data(NULL))
  expect_false(is_data(1))
  expect_false(is_data(''))
  expect_false(is_data(data.frame()))
  expect_false(is_data(data.table(id = numeric()), id = 'id'))
  expect_false(is_data(list(id = 1, time = 1), id = 'id', time = 'time'))
  expect_true(is_data(data.frame(a = 1)))
  expect_false(is_data(data.frame(a = 1), id = 'b')) # missing id column
  expect_true(is_data(data.frame(id = 1), id = 'id'))
  expect_false(is_data(data.frame(id = NA), id = 'id'))
  expect_false(is_data(data.frame(id = 1), id = 'id', time = 'b')) # missing time column
  expect_false(is_data(data.frame(id = 1, time = NA), id = 'id', time = 'time')) # missing time column
  expect_false(is_data(data.frame(id = 1, time = 'time'), id = 'id', time = 'time', response = 'b')) # missing response column
})

test_that('is_newdata', {
  expect_true(is_newdata(NULL))
  expect_true(is_newdata(data.frame()))

  expect_false(is_newdata(numeric()))
  expect_false(is_newdata(NA))
  expect_false(is_newdata(numeric(2)))
  expect_false(is_newdata(list(1, 2)))
})

test_that('is_at', {
  expect_true(is_at(.1))
  expect_true(is_at(0:1))
  expect_true(is_at(c(0, 0)))
  expect_true(is_at(c(-1)))

  expect_false(is_at('0'))
  expect_false(is_at(c(1, NA)))
  expect_false(is_at(c(1, Inf)))
})

test_that('has_colnames', {
  mat = matrix(1:4, ncol = 2)
  expect_true(has_colnames(set_colnames(mat, LETTERS[1:2])))
  expect_true(has_colnames(set_colnames(mat, LETTERS[1:2]), LETTERS[1:2]))

  expect_false(has_colnames(mat))
  expect_false(has_colnames(mat, LETTERS[1:2]))
})

test_that('is_valid_cluster_name', {
  expect_true(is_valid_cluster_name(clusterNames(m1), model = m1))
  expect_true(is_valid_cluster_name(rep(clusterNames(m1), each = 2), model = m1))
  expect_true(is_valid_cluster_name(clusterNames(m1), clusters = clusterNames(m1)))
  expect_true(is_valid_cluster_name(factor(clusterNames(m1)), model = m1))

  expect_false(is_valid_cluster_name('.', model = m1))
  expect_false(is_valid_cluster_name(c(clusterNames(m1), NA), model = m1))
})

test_that('has_same_ids', {
  expect_true(has_same_ids(m1, m2))
})

test_that('has_same_modelData', {
  expect_true(has_same_modelData(m1, m2))
})

test_that('is_class_defined', {
  expect_true(is_class_defined(m1))
})

test_that('has_lcMethod_args', {
  expect_true(has_lcMethod_args(mTest, 'seed'))
  expect_false(has_lcMethod_args(mTest, '..missing'))
})

test_that('is_valid_postprob', {
  expect_true(is_valid_postprob(matrix(1)))
  expect_true(is_valid_postprob(matrix(c(0, 1, 1, 0), nrow = 2)))

  expect_false(is_valid_postprob(NULL))
  expect_false(is_valid_postprob(1))
  expect_false(is_valid_postprob(matrix('1')))
  expect_false(is_valid_postprob(matrix(.5)))

  expect_false(is_valid_postprob(matrix(c(0, 1, 0, 0), nrow = 2))) # a row with sum < 1
  expect_false(is_valid_postprob(matrix(c(1, 1, 1, 0), nrow = 2))) # a row sum > 1
  expect_false(is_valid_postprob(matrix(c(-1, 1, 2, 0), nrow = 2))) # negative entries
})

test_that('no_trajectories_duplicate_time', {
  expect_true(
    no_trajectories_duplicate_time(data.frame(Id = c(1,1,2,2), Time = c(1:2, 1:2)), id = 'Id', time = 'Time')
  )

  expect_true(
    no_trajectories_duplicate_time(data.frame(Id = c(1,1,2), Time = c(1:2, 1)), id = 'Id', time = 'Time')
  )

  expect_true(
    no_trajectories_duplicate_time(testLongData, id = 'Traj', time = 'Assessment')
  )

  expect_false(
    no_trajectories_duplicate_time(data.frame(Id = c(1,1,2,2), Time = c(1, 1, 1:2)), id = 'Id', time = 'Time')
  )
})

test_that('no_empty_trajectories', {
  expect_true(
    no_empty_trajectories(data.frame(Id = 1:3), id = 'Id')
  )

  expect_true(
    no_empty_trajectories(data.frame(Id = factor(LETTERS[1:3])), id = 'Id')
  )

  expect_true(
    no_empty_trajectories(testLongData, id = 'Traj')
  )

  expect_true(
    no_empty_trajectories(data.frame(Id = 1:3), id = 'Id', ids = 1:3)
  )

  expect_false(
    no_empty_trajectories(data.frame(Id = 1:3), id = 'Id', ids = 1:4)
  )

  expect_false(
    no_empty_trajectories(
      data.frame(Id = factor(LETTERS[1:3], levels = LETTERS[1:4])),
      id = 'Id'
    )
  )
})

test_that('have_trajectories_noNA', {
  expect_true(
    have_trajectories_noNA(testLongData, id = 'Traj', response = 'Value')
  )

  expect_false(
    have_trajectories_noNA(copy(testLongData)[2, Value := NA], id = 'Traj', response = 'Value')
  )

  expect_true(
    have_trajectories_noNA(copy(testLongData)[3, Value := Inf], id = 'Traj', response = 'Value')
  )
})

test_that('are_trajectories_length', {
  expect_true(
    are_trajectories_length(testLongData, id = 'Traj', time = 'Assessment', min = 1L)
  )

  expect_true(
    are_trajectories_length(testLongData, id = 'Traj', time = 'Assessment', min = uniqueN(testLongData$Assessment))
  )

  expect_false(
    are_trajectories_length(testLongData, id = 'Traj', time = 'Assessment', min = uniqueN(testLongData$Assessment) + 1L)
  )
})

test_that('are_trajectories_equal_length', {
  expect_true(
    are_trajectories_equal_length(testLongData, id = 'Traj', time = 'Assessment')
  )

  expect_false(
    are_trajectories_equal_length(testLongData[2:.N], id = 'Traj', time = 'Assessment')
  )
})

test_that('no_trajectories_allNA', {
  expect_true(
    no_trajectories_allNA(testLongData, id = 'Traj', response = 'Value')
  )

  expect_false(
    no_trajectories_allNA(testLongData[Traj == unique(Traj)[2], Value := NA], id = 'Traj', response = 'Value')
  )
})
