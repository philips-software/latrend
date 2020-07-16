is.named = function(x) {
  !is.null(names(x))
}

attr(is.named, 'fail') = function(call, env) {
  paste0(deparse(call$x), ' is not named')
}

is.newdata = function(x) {
  is.null(x) || is.list(x) && is.named(x)
}

attr(is.newdata, 'fail') = function(call, env) {
  paste0(deparse(call$x), ' is not valid newdata (list and named, or null)')
}


has_same_ids = function(m1, m2) {
  assert_that(is.lcModel(m1), is.lcModel(m2))
  all.equal(ids(m1), ids(m2)) %>% isTRUE()
}

attr(has_same_ids, 'fail') = function(call, env) {
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

attr(has_same_modelData, 'fail') = function(call, env) {
  m1 = eval(call$m1, env)
  m2 = eval(call$m2, env)
  paste0('models were not trained on the same dataset: ',
         all.equal(model.data(m1), model.data(m2)))
}



#' @export
has_lcMethod_args = function(object, which) {
  assert_that(is.lcMethod(object))

  argNames = setdiff(which, '...')
  all(has_name(object, argNames))
}

attr(has_lcMethod_args, 'fail') = function(call, env) {
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

attr(is_valid_postprob, 'fail') = function(call, env) {
  pp = eval(call$pp, env)
  model = eval(call$model, env)
  validate_that(is.matrix(pp) &&
                  noNA(pp) &&
                  min(pp) >= 0 &&
                  max(pp) <= 1 &&
                  isTRUE(all.equal(rowSums(pp), rep(1, nrow(pp)))))
}
