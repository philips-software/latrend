library(magrittr)
library(assertthat)
library(data.table)
library(latrend)

# Cluster 1: "adherers"
# y = 385, 326, 281
# Cluster 2: "improvers"
# y = 256, 297, 365
# Cluster 3: "non-adherers"
# y = 175, 137, 107

# convert daily usage-hour variance to weekly usage minutes SD
matrix(rnorm(1e6 * 7L, sd = 1.5 * 60), ncol = 1e6L) %>% colMeans() %>% sd()

# define group trajectories
groupTrajMat = cbind(
  Adherers = approx(x = c(1, 4, 13), y = c(385, 328, 281), xout = 1:13, method = 'linear')$y,
  Improvers = approx(x = c(1, 4, 13), y = c(256, 293, 365), xout = 1:13, method = 'linear')$y,
  `Non-adherers` = approx(x = c(1, 4, 13), y = c(176, 138, 108), xout = 1:13, method = 'linear')$y
)

cluster1Data = generateLongData(
  sizes = 162L,
  id = 'Patient',
  cluster = ~ Week + Adherers,
  random = ~ 1,
  data = data.frame(Week = 1:13, Adherers = groupTrajMat[, 'Adherers']),
  clusterCoefs = cbind(c(0, 0, 1)),
  clusterNames = 'Adherers',
  randomScales = cbind(62.7) * .8,
  noiseScales = 23,
  seed = 1L
)

plotTrajectories(cluster1Data, time = 'Week', id = 'Patient', cluster = 'Class', response = 'Value') +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = seq(1, 13, by = 2))

cluster2Data = generateLongData(
  sizes = 56L,
  id = 'Patient',
  cluster = ~ Week + Adherers,
  random = ~ 1,
  data = data.frame(Week = 1:13, Adherers = groupTrajMat[, 'Improvers']),
  clusterCoefs = cbind(c(0, 0, 1)),
  clusterNames = 'Improvers',
  randomScales = cbind(68.9) * .8,
  noiseScales = 43,
  seed = 2L
) %>%
  .[, Patient := Patient + max(cluster1Data$Patient)]

cluster3Data = generateLongData(
  sizes = 83L,
  id = 'Patient',
  cluster = ~ Week + Adherers,
  random = ~ 1,
  data = data.frame(Week = 1:13, Adherers = groupTrajMat[, 'Non-adherers']),
  clusterCoefs = cbind(c(0, 0, 1)),
  clusterNames = 'Non-adherers',
  randomScales = cbind(53.7) * .8,
  noiseScales = 34,
  seed = 3L
) %>%
  .[, Patient := Patient + max(cluster2Data$Patient)]

dataset = rbind(cluster1Data, cluster2Data, cluster3Data) %>%
  .[, UsageHours := fifelse(Value > 0, Value / 60, 0)] %>%
  setnames('Class', 'Group')

plotTrajectories(dataset, time = 'Week', id = 'Patient', cluster = 'Group', response = 'UsageHours') +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = seq(1, 13, by = 2))

PAP.adh = subset(dataset, select = c('Patient', 'Week', 'UsageHours', 'Group')) %>%
  as.data.frame()

head(PAP.adh)
stopifnot(uniqueN(PAP.adh$Patient) == 301)

usethis::use_data(PAP.adh, overwrite = TRUE)
