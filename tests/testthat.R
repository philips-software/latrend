library(testthat)
library(cluslong)
library(data.table)
options(deparse.max.lines=5)
data('testLongData')

test_check('cluslong')
