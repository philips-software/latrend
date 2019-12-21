options(cluslong.time='Assessment')
options(cluslong.id='Subject')

library(stats)
library(utils)
library(data.table)
library(magrittr)

test = function() {
  data('testLongData')
  dt <<- testLongData
  clk <<- clMethodKML(formula=Measurement ~ 1)
  clg <<- clMethodGMM(formula=Measurement ~ Assessment + (1 | ID))
}
