# used internally for creating more readable chained validation statements
`%c%` = function(x, y) {
  c(x, y)
}

warn_that = function(..., msg = NULL, prepend = '', append = '', immediate = TRUE, env = parent.frame()) {
  result = assertthat::see_if(..., env = env, msg = msg)

  if (isTRUE(result)) {
    invisible(result)
  } else {
    warning(prepend, attr(result, 'msg'), append, immediate. = immediate)
  }
}

is_named = function(x) {
  !is.null(names(x)) && noNA(names(x))
}

assertthat::on_failure(is_named) = function(call, env) {
  x = call$x
  if (is.null(x)) {
    paste0(deparse(call$x), ' is not named')
  } else {
    paste0('some elements of ', deparse(call$x), ' are not named')
  }
}

is_newdata = function(x) {
  is.null(x) || is.data.frame(x)
}

assertthat::on_failure(is_newdata) = function(call, env) {
  paste0(deparse(call$x), ' is not valid newdata (data.frame or NULL)')
}

has_colnames = function(x, which) {
  if (missing(which)) {
    !is.null(colnames(x))
  } else {
    all(which %in% colnames(x))
  }
}

assertthat::on_failure(has_colnames) = function(call, env) {
  x = eval(call$x, env)

  if (hasName(call, 'which')) {
    which = eval(call$which, env)
    paste0(
      deparse(call$x),
      ' does not have all of column name(s): "',
      paste0(which, collapse = '", "'),
      '"'
    )
  } else {
    paste0(deparse(call$x), ' does not have any column names')
  }
}

is_at = function(x) {
  is.numeric(x) && noNA(x) && !any(is.infinite(x))
}

assertthat::on_failure(is_at) = function(call, env) {
  x = call$x
  valid = validate_that(
    is.numeric(x),
    noNA(x),
    !any(is.infinite(x))
  )

  paste0('"at" argument of ', deparse(call$x), ' is not valid: ', valid)
}

is_valid_cluster_name = function(x, clusters = clusterNames(model), model) {
  stopifnot(is.character(clusters))

  if (is.factor(x)) {
    noNA(x) && all(levels(x) %in% clusters)
  } else {
    is.character(x) && all(unique(x) %in% clusters)
  }
}

assertthat::on_failure(is_valid_cluster_name) = function(call, env) {
  x = eval(call$x, env)
  clusters = eval(call$clusters, env)

  if (is.character(x) || is.factor(x)) {
    if (anyNA(x)) {
      sprintf('cluster names vector %s should not contain NAs', deparse(call$x))
    } else {
      sprintf(
        'cluster names vector %s contains unexpected elements: expecting "%s"',
        deparse(call$x),
        paste0(clusters, collapse = '", "')
      )
    }
  }
  else {
    sprintf('cluster names vector %s should be character or factor', deparse(call$x))
  }
}

has_same_ids = function(m1, m2) {
  assert_that(is.lcModel(m1), is.lcModel(m2))
  all.equal(ids(m1), ids(m2)) %>% isTRUE()
}

assertthat::on_failure(has_same_ids) = function(call, env) {
  m1 = eval(call$m1, env)
  m2 = eval(call$m2, env)
  paste0('models were not trained on the same ids, or in a different order: ',
         all.equal(ids(m1), ids(m2)))
}


has_same_modelData = function(m1, m2) {
  assert_that(is.lcModel(m1), is.lcModel(m2))
  all.equal(model.data(m1), model.data(m2)) %>%
    isTRUE()
}

assertthat::on_failure(has_same_modelData) = function(call, env) {
  m1 = eval(call$m1, env)
  m2 = eval(call$m2, env)
  paste0('models were not trained on the same dataset: ',
         all.equal(model.data(m1), model.data(m2)))
}


is_class_defined = function(x) {
  isClass(class(x))
}

assertthat::on_failure(is_class_defined) = function(call, env) {
  sprintf('The class "%s" is not defined.\nYou are likely running a custom method or model on a parallel cluster worker without loading the class definitions and methods.',
    class(eval(call$x, env)))
}


#' @export
#' @name latrend-assert
#' @rdname assert
#' @title latrend-specific assertions
#' @description Assertions and checks that may be of use for custom model implementations.
#' @param object The object to test.
#' @param which The argument names. Ellipsis (`...`) will be ignored.
#' @keywords internal
has_lcMethod_args = function(object, which) {
  assert_that(is.lcMethod(object))

  argNames = setdiff(which, '...')
  all(has_name(object, argNames))
}

assertthat::on_failure(has_lcMethod_args) = function(call, env) {
  object = eval(call$object, env)
  argNames = setdiff(eval(call$which, env), '...')
  missingNames = setdiff(argNames, names(object))
  paste0(
    class(object),
    ' is missing required argument(s): ',
    paste0('"', missingNames, '"', collapse = ', ')
  )
}



#' @export
#' @rdname assert
#' @description Check whether the input is a valid posterior probability matrix (for the given model).
#' @param pp The posterior probability `matrix`.
#' @param model The `lcModel` object. Optional.
is_valid_postprob = function(pp, model = NULL) {
  assert_that(is.null(model) || is.lcModel(model))

  clusColsOK = is.null(model) || ncol(pp) == nClusters(model)

  is.matrix(pp) &&
    is.numeric(pp) &&
    clusColsOK &&
    noNA(pp) &&
    min(pp) >= 0 &&
    max(pp) <= 1 &&
    isTRUE(all.equal(
      rowSums(pp),
      rep(1, nrow(pp)),
      check.attributes = FALSE,
      use.names = FALSE
    ))
}

assertthat::on_failure(is_valid_postprob) = function(call, env) {
  pp = eval(call$pp, env)
  model = eval(call$model, env)

  if (!is.null(model)) {
    clusVal = validate_that(ncol(pp) == nClusters(model))
    if (clusVal) {
      return(clusVal)
    }
  }

  validate_that(
    is.matrix(pp) &&
    is.numeric(pp) &&
    noNA(pp) &&
    min(pp) >= 0 &&
    max(pp) <= 1 &&
    isTRUE(all.equal(rowSums(pp), rep(1, nrow(pp))))
  )
}

#' @export
#' @rdname assert
#' @description Check whether the dataset does not contain trajectories without any observations.
#' Requires Id column to be factor.
#' @param ids Optional `character vector` of trajectory identifiers that are expected to be present in the data.
no_empty_trajectories = function(data, id, ids) {
  assert_that(
    is.data.frame(data),
    is.string(id),
    has_name(data, id),
    noNA(data[[id]])
  )

  if (missing(ids)) {
    ids = levels(data[[id]])
  }

  if (length(ids) == 0) {
    TRUE
  } else {
    all(ids %in% data[[id]])
  }
}

assertthat::on_failure(no_empty_trajectories) = function(call, env) {
  data = eval(call$data, env) %>% as.data.table()
  id = eval(call$id, env)

  if (hasName(call, 'ids')) {
    ids = eval(call$ids, env)
  } else {
    ids = levels(data[[id]])
  }

  missingIds = setdiff(ids, unique(data[[id]]))

  sprintf(
    'The dataset contains %d trajectories that have no observations:\n  %s',
    length(missingIds),
    paste0('"', missingIds, '"', collapse = ', ')
  )
}

#' @export
#' @rdname assert
#' @description Check whether the provided `data.frame` represents a longitudinal dataset
#' @param id The trajectory identifier column name. Optional.
#' @param time The time column name. Optional.
#' @param response The response column name. Optional.
is_data = function(data, id, time, response) {
  stopifnot(
    missing(id) || is.string(id),
    missing(time) || is.string(time),
    missing(response) || is.string(response)
  )

  all(
    is.data.frame(data),
    nrow(data) > 0,
    missing(id) || hasName(data, id) && noNA(data[[id]]),
    missing(time) || hasName(data, time) && noNA(data[[time]]),
    missing(response) || hasName(data, response)
  )
}

assertthat::on_failure(is_data) = function(call, env) {
  data = eval(call$data, env)

  if (!is.data.frame(data)) {
    return ('Dataset must be a data.frame')
  } else if (nrow(data) == 0) {
    return ('Dataset must contain at least 1 row')
  }

  if (hasName(call, 'id')) {
    id = eval(call$id, env)
    if (!hasName(data, id)) {
      return (sprintf('Dataset is missing id column "%s"', id))
    } else if (anyNA(data[[id]])) {
      return (sprintf('Dataset id column "%s" contains NAs', id))
    }
  }

  if (hasName(call, 'time')) {
    time = eval(call$time, env)
    if (!hasName(data, time)) {
      return (sprintf('Dataset is missing time column "%s"', time))
    } else if (anyNA(data[[time]])) {
      return (sprintf('Dataset time column "%s" contains NAs', time))
    }
  }

  if (hasName(call, 'response')) {
    response = eval(call$response, env)
    if (!hasName(data, response)) {
      return (sprintf('Dataset is missing response column "%s"', response))
    }
  }

  stop('uncaught assertion failure. please report')
}

#' @export
#' @rdname assert
#' @description Check whether the dataset does not contain trajectories with duplicate observation moments.
no_trajectories_duplicate_time = function(data, id, time) {
  assert_that(is_data(data, id, time))

  all(as.data.table(data)[, anyDuplicated(get(..time)) == 0, by = c(id)]$V1)
}

assertthat::on_failure(no_trajectories_duplicate_time) = function(call, env) {
  data = eval(call$data, env) %>% as.data.table()
  id = eval(call$id, env)
  time = eval(call$time, env)

  dtTraj = data[, .(Dupe = anyDuplicated(get(..time)) > 0), by = c(id)] %>%
    .[Dupe == TRUE]

  sprintf(
    'The dataset contains %d trajectories that have duplicate observation moments:\n Ids: %s',
    nrow(dtTraj),
    paste0('"', dtTraj[[id]], '"', collapse = ', ')
  )
}

#' @export
#' @rdname assert
#' @description Check the number of observation moments for each trajectory
#' @param min The minimum required number.
are_trajectories_length = function(data, min = 1, id, time) {
  assert_that(
    is_data(data, id, time),
    is.count(min + 1L)
  )

  data = as.data.table(data)
  all(data[, uniqueN(get(time)), by = c(id)]$V1 >= min)
}

assertthat::on_failure(are_trajectories_length) = function(call, env) {
  data = eval(call$data, env) %>% as.data.table()
  id = eval(call$id, env)
  time = eval(call$time, env)
  min = eval(call$min, env)

  dtTraj = data[, .(Moments = uniqueN(get(time))), by = c(id)] %>%
    .[Moments < min]

  sprintf(
    'The dataset contains %d trajectories that have fewer than %d observations moments.\n Ids:\n  %s',
    nrow(dtTraj),
    min,
    paste0('"', as.character(dtTraj[[id]]), '"', collapse = ', ')
  )
}

#' @export
#' @rdname assert
#' @description Check whether all trajectories have the same number of observation moments, and are observed at the same moments in time.
#' @param id The id variable
#' @param time The time variable
are_trajectories_equal_length = function(data, id, time) {
  assert_that(is_data(data, id, time))
  data = as.data.table(data)
  nTimes = uniqueN(data[[time]])

  all(data[, .N == nTimes && uniqueN(get(time)) == nTimes, by = c(id)]$V1)
}

assertthat::on_failure(are_trajectories_equal_length) = function(call, env) {
  data = eval(call$data, env) %>% as.data.table()
  method = eval(call$method, env)
  id = eval(call$id, env)
  time = eval(call$time, env)

  nTimes = uniqueN(data[[time]])
  # check for trajectories with multiple observations at the same moment in time
  dtMult = data[, .(HasMult = anyDuplicated(get(time))), by = c(id)] %>%
    .[HasMult == TRUE]

  if (any(dtMult$HasMult)) {
    sprintf(
      'The dataset contains %d trajectories that have multiple observations at the same moment in time.\n Ids:\n  %s',
      nroW(dtMult),
      as.character(dtMult[[id]])
    )
  } else {
    # check for equal length, which implies identical observation times
    dtLen = data[, .(IsEqualLen = .N == nTimes)] %>%
      .[IsEqualLen == FALSE]
    sprintf(
      'The dataset contains %d trajectories that are of a different length than %d or have different observation times, whereas all trajectories are required to be fully aligned in time and length.\n Ids:\n  %s',
      nrow(dtLen),
      nTimes,
      as.character(dtLen[[id]])
    )
  }
}

#' @export
#' @rdname assert
#' @description Check whether all trajectories don't contain any NA observations.
have_trajectories_noNA = function(data, id, response) {
  assert_that(is_data(data, id, response = response))

  noNA(data[[response]])
}

assertthat::on_failure(have_trajectories_noNA) = function(call, env) {
  data = eval(call$data, env) %>% as.data.table()
  method = eval(call$method, env)
  id = eval(call$id, env)
  response = eval(call$response, env)

  dtMissing = data[, .(NaCount = sum(is.na(get(response)))), by = c(id)] %>%
    .[NaCount > 0]

  sprintf(
    'The dataset contains %d trajectories with NA observations in "%s". To fix this, either remove or impute these observations.\nList of trajectories that contain NA observations:\n  %s',
    nrow(dtMissing),
    response,
    paste0('"', as.character(dtMissing[[id]]), '" (', dtMissing$NaCount, ')', collapse = ', ')
  )
}

.warnIfTrajLength = function(method, data, n = getOption('latrend.warnTrajectoryLength', default = 1)) {
  if (n > 0) {
    id = idVariable(method)
    time = timeVariable(method)
    assert_that(has_name(data, c(id, time)))
    trajObs = subset(data, select = c(id, time)) %>%
      as.data.table() %>%
      .[, .(N = uniqueN(get(time))), by = c(id)]

    lowIds = trajObs[N <= n, get(id)]
    if (length(lowIds) > 0) {
      warning(
        sprintf(
          'The dataset contains %d trajectories having %d observation(s) or fewer.\nThis warning can be disabled using options(latrend.warnTrajectoryLength = 0).\nFlagged trajectories: %s',
          length(lowIds),
          n,
          paste0('"', lowIds, '"', collapse = ', ')
        )
      )
    }
  }
}
