#' @include modelCustom.R
setClass('lcModelFeature', contains = 'lcModelCustom')

#' @rdname interface-featureBased
setMethod('getName', signature('lcModelFeature'), function(object, ...)
  getLcMethod(object) %>% getName)

#' @rdname interface-featureBased
setMethod('getShortName', signature('lcModelFeature'), function(object, ...)
  getLcMethod(object) %>% getShortName)
