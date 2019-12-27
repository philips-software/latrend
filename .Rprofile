options(cluslong.time='Assessment')
options(cluslong.id='Subject')

library(stats)
library(utils)
library(data.table)
library(magrittr)

test = function() {
  data('testLongData')
  dat <<- testLongData
  clk <<- clMethodKML(formula=Measurement ~ 1)
  clp <<- clMethodLLPA(formula=Measurement ~ 1)
  clb <<- clMethodGBTM(formula=Measurement ~ Assessment)
  clg <<- clMethodGMM(formula=Measurement ~ CLUSTER * Assessment + (1 | ID))
}
