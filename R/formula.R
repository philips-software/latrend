hasResponse = function(f) {
  terms(f, 'response') != 0
}

hasSingleResponse = function(f) {
  hasResponse(f) && length(all.vars(update(f, . ~ 1))) == 1
}

getResponse = function(f) {
  all.vars(update(f, . ~ 1)[[1]])
}

getCovariates = function(f) {
  labels(f)
}

hasCovariates = function(f) {
  length(getCovariates(f)) == 0
}