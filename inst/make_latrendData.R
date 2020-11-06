library(usethis)
library(magrittr)
library(latrend)

# Example data ####
set.seed(1)
latrendData = generateLongData(
  sizes = c(90, 80, 30),
  fixed = Y ~ 1,
  cluster = ~ Time + I(Time^2),
  random = ~ Time,
  id = 'Id',
  data = data.frame(Time = seq(0, 2, length.out = 10)),
  fixedCoefs = 0,
  clusterCoefs = cbind(c(-1, 1.5, -.4), c(0, 0, 0), c(.25, .25, -.4)),
  randomScales = cbind(c(.3, .05), c(.2, .1), c(.1, .1)), #cbind(c(0,0), c(0,0), c(0,0)),
  noiseScales = c(.1, .1, .1),
  clusterNames = paste('Class', 1:3),
  shuffle = FALSE) %>%
  .[, .(Id, Time, Y, Class)] %>%
  as.data.frame()

plotTrajectories(latrendData, response = 'Y')
plotTrajectories(latrendData, response = 'Y', cluster = 'Class', facet = FALSE)
plotTrajectories(latrendData, response = 'Y', cluster = 'Class', facet = TRUE)

use_data(latrendData, overwrite = TRUE)
