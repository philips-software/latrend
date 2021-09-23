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
      return (clusVal)
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
