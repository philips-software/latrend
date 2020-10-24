is.named = function(x) {
  !is.null(names(x))
}

on_failure(is.named) = function(call, env) {
  paste0(deparse(call$x), ' is not named')
}

is.newdata = function(x) {
  is.null(x) || is.list(x) && is.named(x)
}

on_failure(is.newdata) = function(call, env) {
  paste0(deparse(call$x), ' is not valid newdata (list and named, or null)')
}


has_same_ids = function(m1, m2) {
  assert_that(is.lcModel(m1), is.lcModel(m2))
  all.equal(ids(m1), ids(m2)) %>% isTRUE()
}

on_failure(has_same_ids) = function(call, env) {
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

on_failure(has_same_modelData) = function(call, env) {
  m1 = eval(call$m1, env)
  m2 = eval(call$m2, env)
  paste0('models were not trained on the same dataset: ',
         all.equal(model.data(m1), model.data(m2)))
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

on_failure(has_lcMethod_args) = function(call, env) {
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
#' @description Check whether the input is a valid posterior probability matrix for the given model.
#' @param pp The posterior probability `matrix`.
#' @param model The `lcModel` object.
is_valid_postprob = function(pp, model) {
  assert_that(is.lcModel(model))
  is.matrix(pp) &&
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

on_failure(is_valid_postprob) = function(call, env) {
  pp = eval(call$pp, env)
  model = eval(call$model, env)
  validate_that(is.matrix(pp) &&
                  noNA(pp) &&
                  min(pp) >= 0 &&
                  max(pp) <= 1 &&
                  isTRUE(all.equal(rowSums(pp), rep(1, nrow(pp)))))
}
