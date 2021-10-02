context('matching calls')

test_that('provided argument', {
  f = function(a) {
    match.call.all()
  }

  out = f(a = 1)
  expect_is(out, 'call')
  expect_named(out, c('', 'a'))
  expect_equal(out$a, 1)
})

test_that('provided expr argument', {
  f = function(a) {
    match.call.all()
  }

  out = f(a = mean(z) > 1)
  expect_is(out, 'call')
  expect_named(out, c('', 'a'))
  expect_equal(deparse(out$a), 'mean(z) > 1')
})

test_that('provided negative numeric argument', {
  f = function(a) {
    match.call.all()
  }

  out = f(a = -1)
  expect_is(out$a, 'call')
  expect_equal(deparse(out$a), '-1')
})

test_that('provided int argument', {
  f = function(a) {
    match.call.all()
  }

  out = f(a = 1L)
  expect_is(out$a, 'integer')
  expect_equal(out$a, 1L)
})

test_that('default numeric argument', {
  f = function(a = 1) {
    match.call.all()
  }

  out = f()
  expect_is(out, 'call')
  expect_named(out, c('', 'a'))
  expect_equal(out$a, 1)
})

test_that('default negative numeric argument', {
  f = function(a = -1) {
    match.call.all()
  }

  out = f()
  expect_is(out$a, 'call')
  expect_equal(deparse(out$a), '-1')
})

test_that('default int argument', {
  f = function(a = 1L) {
    match.call.all()
  }

  out = f()
  expect_is(out$a, 'integer')
  expect_equal(out$a, 1L)
})

test_that('default expr argument', {
  f = function(a = mean(z) > 1) {
    match.call.all()
  }

  out = f()
  expect_is(out, 'call')
  expect_named(out, c('', 'a'))
  expect_is(out$a, 'call')
  expect_equal(deparse(out$a), 'mean(z) > 1')
})

test_that('overwriting default numeric argument', {
  f = function(a = 1) {
    match.call.all()
  }

  out = f(a = 2)
  expect_is(out, 'call')
  expect_named(out, c('', 'a'))
  expect_equal(out$a, 2)
})

test_that('provided argument, and a default', {
  f = function(a, b = 2) {
    match.call.all()
  }

  out = f(a = 1)
  expect_is(out, 'call')
  expect_named(out, c('', 'a', 'b'))
  expect_equal(out$a, 1)
  expect_equal(out$b, 2)
})

test_that('nested with single argument', {
  f = function(a) {
    g = function(a = 1, ...) {
      match.call.all()
    }
    g(z = a)
  }

  out = f(a = mean)
  expect_named(out, c('', 'a', 'z'))
  expect_equal(out$a, 1)
  expect_is(out$z, 'name')
  expect_equal(deparse(out$z), 'a')
})

test_that('nested with variable arguments', {
  f = function(...) {
    g = function(a, ...) {
      match.call.all()
    }
    g(...)
  }

  out = f(a = mean(z) > 1, b = 1)
  expect_named(out, c('', 'a', 'b'))
  expect_equal(deparse(out$a), 'mean(z) > 1')
  expect_equal(out$b, 1)
})

test_that('stratify method', {
  m = lcMethodTestStratify(stratify = mean(Value) > 1)
  expect_equal(deparse(m[['stratify', eval = FALSE]]), 'mean(Value) > 1')
})
