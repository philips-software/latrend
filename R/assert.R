# used internally for creating more readable chained validation statements
`%c%` = function(x, y) {
  c(x, y)
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
#' @description Check whether all trajectories have the same number of observations, and are observed at the same moments in time.
#' @param id The id variable
#' @param time The time variable
are_trajectories_equal_length = function(data, id, time) {
  assert_that(
    is.data.frame(data),
    is.string(id),
    is.string(time)
  )
  data = as.data.table(data)
  stopifnot(noNA(data[[time]]))
  nTimes = uniqueN(data[[time]])
  all(data[, .N == nTimes, by = c(id)]$V1)
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
      'Data contains %d trajectories that have multiple observations at the same moment in time.\nList of problematic trajectories:\n  %s',
      nroW(dtMult),
      as.character(dtMult[[id]])
    )
  } else {
    # check for equal length, which implies identical observation times
    dtLen = data[, .(IsEqualLen = .N == nTimes)] %>%
      .[IsEqualLen == FALSE]
    sprintf(
      'Data contains %d trajectories that are of a different length than %d or have different observation times, whereas all trajectories are required to be fully aligned in time and length.\nList of problematic trajectories:\n  %s',
      nroW(dtLen),
      nTimes,
      as.character(dtLen[[id]])
    )
  }
}

#' @export
#' @rdname assert
#' @description Check whether all trajectories don't contain any NA observations.
have_trajectories_noNA = function(data, id, response) {
  assert_that(
    is.data.frame(data),
    is.string(id),
    is.string(response)
  )
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
    'Data contains %d trajectories with NA observations in "%s". To fix this, either remove or impute these observations.\nList of trajectories that contain NA observations:\n  %s',
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
          'Input dataset contains %d trajectories having %d observation(s) or fewer.\nThis warning can be disabled using options(latrend.warnTrajectoryLength = 0).\nFlagged trajectories: %s',
          length(lowIds),
          n,
          paste0('"', lowIds, '"', collapse = ', ')
        )
      )
    }
  }
}
