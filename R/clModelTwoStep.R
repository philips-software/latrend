#' @include clModelCustom.R
setClass('clModelTwoStep', contains='clModelCustom')

setMethod('getName', signature('clModelTwoStep'), function(object) getMethod(object) %>% getName)

setMethod('getName0', signature('clModelTwoStep'), function(object) getMethod(object) %>% getName0)
