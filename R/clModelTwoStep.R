#' @include clModelCustom.R
setClass('clModelTwoStep', contains='clModelCustom')

setMethod('getName', signature('clModelTwoStep'), function(object) getMethod(object) %>% getName)

setMethod('getShortName', signature('clModelTwoStep'), function(object) getMethod(object) %>% getShortName)
