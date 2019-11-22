.onLoad = function(libname, pkgname) {
  opts = list(
    cluslong.time='Time',
    cluslong.id='Id')

  optMask = not(names(opts) %in% names(options()))
  if(any(optMask)) {
    options(opts[optMask])
    packageStartupMessage('Default options:')
    packageStartupMessage(paste0('\t', names(opts[optMask]), ' = ', opts[optMask], collapse='\n'))
  }
}

#.onAttach = function(libname, pkgname) {
#}