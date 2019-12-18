is.formula = function(x) {
  inherits(x, 'formula')
}

hasResponse = function(f) {
  terms(f) %>% attr('response') != 0
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

getCovariates = function(f) {
  update(f, NULL ~ .) %>% all.vars
}

hasCovariates = function(f) {
  length(getCovariates(f)) == 0
}