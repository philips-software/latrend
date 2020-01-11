context('clMethod')

test_that('creation', {
  xvar = 2
  method = new('clMethod', call=call('clMethod', a=1, b='a', c=NULL, d=NA, e=quote(xvar)))

  expect_is(getCall(method), 'call')
  expect_equal(names(method), letters[1:5])
  expect_output(show(method))
  expect_output(print(method))
})

test_that('unevaluated creation', {
  method = new('clMethod', call=call('clMethod', e=quote(xvar)))
  expect_error(method$e)
  expect_output(show(method))
  expect_output(print(method))
})

test_that('length', {
  new('clMethod', call=call('clMethod')) %>%
    expect_length(0)

  new('clMethod', call=call('clMethod', a=1)) %>%
    expect_length(1)

  new('clMethod', call=call('clMethod', a=1, e=quote(xvar))) %>%
    expect_length(2)
})

test_that('argument retrieval', {
  xvar = 2
  method = new('clMethod', call=call('clMethod', a=1, b='a', c=NULL, d=NA, e=quote(xvar)))

  expect_equal(method$a, 1)
  expect_equal(method$b, 'a')
  expect_null(method$c)
  expect_true(is.na(method$d))
  expect_equal(method$e, xvar)
  expect_error(method$missing)
  expect_equal(method[['a']], method$a)
  expect_error(method[['missing']])

  expect_is(method[['e', eval=FALSE]], 'name')
})

test_that('environment()', {
  method = new('clMethod', call=call('clMethod', e=quote(xvar)))
  e = new.env()
  e$xvar = 3

  expect_error(method$e)
  environment(method) = e
  expect_equal(method$e, e$xvar)
})

test_that('local variables', {
  f = function() {
    xvar = 2
    new('clMethod', call=call('clMethod', e=quote(xvar)))
  }
  expect_error(f()$e) # value of xvar is not defined

  g = function() {
    xvar = 2
    m = new('clMethod', call=call('clMethod', e=quote(xvar)))
    xvar = 3
    m$e #should be 3
  }
  expect_equal(g(), 3)
})

test_that('variable from custom environment', {
  method = new('clMethod', call=call('clMethod', e=quote(xvar)))
  expect_error(method$e)

  e = new.env()
  e$xvar = 2
  expect_error(method$e)
  expect_equal(method[['e', envir=e]], 2)
})

test_that('formula', {
  method = new('clMethod', call=call('clMethod', formula=A~B, formula.sigma=~C))
  expect_is(formula(method), 'formula')
  expect_error(formula(method, 'missing'))
  expect_equal(formula(method), A~B)
  expect_equal(formula(method, 'sigma'), ~C)
})

test_that('update', {
  xvar = 2
  method = new('clMethod', call=call('clMethod', a=1, b='a', c=NULL, d=NA, e=xvar))

  expect_equal(update(method, a=2)$a, 2)
  expect_null(update(method, a=NULL)$a)
  expect_equal(update(method, c=2)$c, 2)
  expect_error(update(method, missing=1))
})

test_that('update.clMethod with local variables', {
  xvar = 2
  method = new('clMethod', call=call('clMethod', e=quote(xvar)))
  u = update(method, e=xvar)
  xvar = 3
  expect_equal(u$e, 3)
})

test_that('dependency function evaluation', {
  method = clMethodKML()
  expect_is(method$center, 'function')
})

test_that('as.list', {
  xvar = 2
  method = new('clMethod', call=call('clMethod', a=1, b='a', c=NULL, d=NA, e=quote(xvar)))
  xvar = 3
  expect_equal(as.list(method), list(a=1, b='a', c=NULL, d=NA, e=xvar))
})

test_that('substitute', {
  xvar = 2
  method = new('clMethod', call=call('clMethod', a=1, b='a', c=NULL, d=NA, e=quote(xvar)))
  method2 = substitute.clMethod(method)

  expect_equal(method2[['a', eval=FALSE]], 1)
  expect_null(method2[['c', eval=FALSE]])
  expect_equal(method2[['e', eval=FALSE]], 2)
})