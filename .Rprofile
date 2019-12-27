library(stats)
library(utils)
library(data.table)
library(magrittr)

test = function() {
  data('testLongData')
  dat <<- generateLongData()

  clk <<- clMethodKML(formula=Value ~ 1)
  clp <<- clMethodLLPA(formula=Value ~ 1)
  clb <<- clMethodGBTM(formula=Value ~ Time)
  clg <<- clMethodGMM(formula=Value ~ CLUSTER * Time + (1 | ID))
}
