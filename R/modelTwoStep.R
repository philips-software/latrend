#' @include modelCustom.R
setClass('lcModelTwoStep', contains = 'lcModelCustom')

#' @rdname interface-featureBased
setMethod('getName', signature('lcModelTwoStep'), function(object, ...)
  getLcMethod(object) %>% getName)

#' @rdname interface-featureBased
setMethod('getShortName', signature('lcModelTwoStep'), function(object, ...)
  getLcMethod(object) %>% getShortName)
