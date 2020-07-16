#' @include modelCustom.R
setClass('lcModelTwoStep', contains = 'lcModelCustom')

setMethod('getName', signature('lcModelTwoStep'), function(object)
  getLcMethod(object) %>% getName)

setMethod('getShortName', signature('lcModelTwoStep'), function(object)
  getLcMethod(object) %>% getShortName)
