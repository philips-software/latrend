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
  assert_that(is.clModel(m1), is.clModel(m2))
  all.equal(ids(m1), ids(m2)) %>% isTRUE()
}

attr(has_same_ids, 'fail') = function(call, env) {
  m1 = eval(call$m1, env)
  m2 = eval(call$m2, env)
  paste0('models were not trained on the same ids, or in a different order: ', all.equal(ids(m1), ids(m2)))
}

has_same_data = function(m1, m2) {
  assert_that(is.clModel(m1), is.clModel(m2))
  all.equal(model.data(m1), model.data(m2)) %>% isTRUE()
}

attr(has_same_data, 'fail') = function(call, env) {
  m1 = eval(call$m1, env)
  m2 = eval(call$m2, env)
  paste0('models were not trained on the same dataset: ', all.equal(model.data(m1), model.data(m2)))
}
