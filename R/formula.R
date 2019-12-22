is.formula = function(x) {
  inherits(x, 'formula')
}

hasResponse = function(f) {
  if (is.formula(f)) {
    tt = terms(f)
  } else {
    tt = f
  }
  attr(tt, 'response') != 0
}

hasIntercept = function(f) {
  if (is.formula(f)) {
    tt = terms(f)
  } else {
    tt = f
  }
  attr(tt, 'intercept') != 0
}

hasSingleResponse = function(f) {
  hasResponse(f) && length(all.vars(update(f, . ~ 1))) == 1
}

getResponse = function(f) {
  if (hasResponse(f)) {
    update(f, . ~ 1) %>% all.vars %>% head(1)
  }
  else {
    return(NULL)
  }
}

#' @importFrom lme4 findbars
getREterms = function(f) {
  terms = lme4::findbars(f)
}

getREFormula = function(term) {
  assert_that(is.call(term))
  as.character(term)[2] %>% reformulate
}

getREGroupName = function(term) {
  assert_that(is.call(term))
  as.character(term)[3]
}

getCovariates = function(f) {
  update(f, NULL ~ .) %>% all.vars
}

hasCovariates = function(f) {
  length(getCovariates(f)) > 0
}

dropResponse = function(f) {
  update(f, NULL ~ .)
}

dropIntercept = function(f) {
  update(f, . ~ -1)
}

#' @title Drop random-effects component from a formula
#' @description Remove the random-effects components specified by "(. | .)" from a formula
dropRE = function(f) {
  reStrings = getREterms(f) %>% as.character
  reIdx = match(reStrings, labels(terms(f)))
  assert_that(!anyNA(reIdx))
  drop.terms(terms(f), reIdx, keep.response=hasResponse(f)) %>% formula
}

# CLUSTER specific ####
# Drop any terms that have an interaction with CLUSTER
dropCLUSTER = function(f) {
  tt = terms(f)
  vars = labels(tt)
  newvars = vars[!startsWith(vars, 'CLUSTER:') & !endsWith(vars, ':CLUSTER') & vars != 'CLUSTER']
  reformulate(termlabels=newvars,
              intercept=attr(tt, 'intercept'),
              response=getResponse(f),
              env=attr(f, '.Environment'))
}

# Keep only terms that have an interaction with CLUSTER
keepCLUSTER = function(f) {
  tt = terms(f)
  vars = labels(tt)
  vars1 = vars[startsWith(vars, 'CLUSTER:')] %>% substring(first=9)
  rmstr = function(x) {
    substr(x, start=0, stop=nchar(x) - 8)
  }
  vars2 = vars[endsWith(vars, ':CLUSTER')] %>% rmstr

  reformulate(termlabels=c(vars1, vars2),
              intercept='CLUSTER' %in% vars,
              response=getResponse(f),
              env=attr(f, '.Environment'))
}