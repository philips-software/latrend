context('data')

test_that('testLongData', {
  data('testLongData')
  expect_is(testLongData, 'data.frame')
  expect_named(testLongData, c('Id', 'Time', 'Value', 'Cluster'))
  expect_length(unique(testLongData$Id), 100)
  expect_length(unique(testLongData$Time), 11)
})

test_that('namedTestData', {
  expect_is(namedTestData, 'data.frame')
  expect_named(namedTestData, c('Subject', 'Assessment', 'Amount', 'Cluster'))
})

test_that('generateLongData without fixed and random effects', {
  dt = generateLongData(sizes=c(1, 2), fixed=Value ~ 0, cluster=~1, random=~0, id='Id',
                   data=data.frame(Time=c(0,1)),
                   fixedCoefs=NULL, clusterCoefs=cbind(1,2), randomScales=NULL)
  expect_is(dt, 'data.frame')
  expect_equal(nrow(dt), 6)
  expect_true(all(dt$Mu.fixed == 0))
  expect_equal(dt$Mu.cluster, rep(1:2, c(2, 4)))
  expect_equal(dt$Mu, rep(1:2, c(2, 4)))
  expect_false(has_name(dt, 'Mu.random'))
})

test_that('generateLongData with fixed effects', {
  dt = generateLongData(sizes=c(1, 1), fixed=Value ~ Time, cluster=~1, random=~0, id='Id',
                        data=data.frame(Time=c(0,.5,1)),
                        fixedCoefs=c(1,2), clusterCoefs=cbind(1,2), randomScales=NULL)
  expect_is(dt, 'data.frame')
  expect_length(dt, 7)
  expect_equal(nrow(dt), 6)
  expect_equal(dt$Mu.fixed, rep(1:3, 2))
  expect_equal(dt$Mu.cluster, rep(1:2, each=3))
  expect_equal(dt$Mu, c(2:4, 3:5))
  expect_false(has_name(dt, 'Mu.random'))
})

test_that('generateLongData with random intercept', {
  set.seed(1)
  dt = generateLongData(sizes=c(10, 10), fixed=Value ~ Time, cluster=~1, random=~1, id='Id',
                        data=data.frame(Time=c(0,.5,1)),
                        fixedCoefs=c(1,2), clusterCoefs=cbind(1,2), randomScales=cbind(.1,10))
  expect_true(has_name(dt, 'Mu.random'))
  expect_equal(dt[Time == 0, round(log10(sd(Mu))), by=Cluster]$V1, c(-1, 1))
})

test_that('generateLongData with multiple terms', {
  set.seed(1)
  dt = generateLongData(sizes=c(1, 1),
                        fixed=Value ~ poly(Time, 2, raw=TRUE),
                        cluster=~poly(Time, 2, raw=TRUE),
                        random=~poly(Time, 2, raw=TRUE),
                        id='Id',
                        data=data.frame(Time=c(0,.5,1)),
                        fixedCoefs=c(1,2,3),
                        clusterCoefs=cbind(c(1,1,1),c(2,2,2)),
                        randomScales=cbind(c(.1,1,2), c(10,1,1)))
  expect_equal(nrow(dt), 6)
  expect_length(dt, 10)
  expect_equal(dt$Mu.cluster, c(1, 1.75, 3, 2, 3.5, 6))
})