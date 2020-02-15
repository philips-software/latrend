library(assertthat)
library(data.table)
library(magrittr)

#' @description Generates data from the group definitions provided by M. Aloia et al. (2008).
#' @param patients Number of patients
#' @param times Times at which to generate the data points
#' @param nAggr Number of measurements per bin
#' @param props Group proportions
#' @param N Average number of measurements
#' @param int Level
#' @param slope Slope
#' @param var Measurement variance
#' @param r Autocorrelation
#' @param missing Whether to simulate measurements being missing (including drop-out)
#' @param seed The seed for the PRNG
#' @references
#' Mark S. Aloia, Matthew S. Goodwin, Wayne F. Velicer, J. Todd Arnedt, Molly Zimmerman, Jaime Skrekas, Sarah Harris, Richard P. Millman,
#' Time Series Analysis of Treatment Adherence Patterns in Individuals with Obstructive Sleep Apnea, Annals of Behavioral Medicine,
#' Volume 36, Issue 1, August 2008, Pages 44–53, https://doi.org/10.1007/s12160-008-9052-9
generate_osa_data = function(patients=500,
                             times=seq(1, 365, by=1),
                             nAggr=14,
                             props=c(GU=.24, SI=.13, SD=.14, VU=.17, OA=.08, ED=.13, NU=.11),
                             # N=c(354, 344, 280, 299, 106, 55, 10),
                             # sd.N=c(31, 49, 68, 55, 64, 30, 4),
                             dropoutTimes=c(Inf, Inf, Inf, Inf, Inf, 80, 20),
                             sd.dropoutTimes=c(0, 0, 0, 0, 0, 30, 10),
                             attemptProbs=c(354, 344, 280, 299, 106, 55, 14) / pmin(dropoutTimes, 365), # based on median
                             intercepts=c(GU=6.6, SI=5.8-1, SD=6.1, VU=4.9-.5, OA=3.2, ED=4.0, NU=2.5),
                             sd.intercepts=c(GU=.81, SI=1.6-.1, SD=.95, VU=1.3, OA=1.6, ED=1.6, NU=1.4),
                             slopes=c(GU=0, SI=.0058*3, SD=-.0038*5, VU=.0004*24, OA=-.003, ED=-.0014, NU=-.015),
                             sd.slopes=c(GU=.0016, SI=.0031/2, SD=.0027/2, VU=.0032*0, OA=.0091, ED=.01, NU=.01),
                             quads=c(GU=0, SI=-.00003, SD=.00003, VU=-.00003, OA=0, ED=-.0001, NU=-.0001),
                             sd.quads=c(GU=0, SI=0, SD=0, VU=0, OA=0, ED=0, NU=0),
                             vars=c(GU=2.0, SI=3.6, SD=3.2, VU=3.4, OA=3.6, ED=5.0, NU=3.0),
                             sd.vars=c(.82, 1.3, .85, 1.2, 1.8, 2.6, 1.7),
                             autocors=c(GU=.056, SI=.11, SD=.073, VU=.048, OA=.006, ED=-.044, NU=-.31),
                             groupNames=c('Good users', 'Slow improvers', 'Slow decliners', 'Variable users', 'Occasional attempters', 'Early drop-outs', 'Non-users'),
                             missing=FALSE,
                             seed=1) {
    set.seed(seed)
    groupCounts = floor(patients * props)
    incrIdx = order((patients * props) %% 1) %>%
        head(patients - sum(groupCounts))
    groupCounts[incrIdx] = groupCounts[incrIdx] + 1 # increment the groups that were closest to receiving another patient
    assert_that(sum(groupCounts) == patients)
    groupNames = factor(groupNames, levels=groupNames)

    # generate patient coefficients
    groupCoefs = data.table(Group=groupNames, Patients=groupCounts,
                            TDrop=dropoutTimes, Sd.TDrop=sd.dropoutTimes,
                            AProb=attemptProbs,
                            Int=intercepts, Sd.Int=sd.intercepts,
                            Slope=slopes, Sd.Slope=sd.slopes,
                            Quad=quads, Sd.Quad=sd.quads,
                            Var=vars, Sd.Var=sd.vars,
                            R=autocors)
    assert_that(nrow(groupCoefs) == 7)
    patCoefs = groupCoefs[, .(TDrop=rnorm(Patients, TDrop, Sd.TDrop) %>% round %>% pmax(7),
                              AProb=AProb,
                              Intercept=rnorm(Patients, Int, Sd.Int) %>% pmax(0),
                              Slope=rnorm(Patients, Slope, Sd.Slope),
                              Quad=rnorm(Patients, Quad, Sd.Quad),
                           Variance=rnorm(Patients, Var, Sd.Var) %>% pmax(.75),
                           R=rep(R, Patients)), by=Group]

    # generate patient measurements
    genTs = function(N, Intercept, Slope, Quad, Variance, R, AProb, TDrop, ...) {
        y = as.numeric(Intercept + times * Slope + times^2 * Quad + arima.sim(list(ar=R), n=length(times), sd=sqrt(Variance)))

        skipMask = !rbinom(length(times), size=1, prob=AProb)
        y[skipMask] = 0

        if(missing) {
            obsMask = times <= TDrop
            list(Time=times[obsMask], Usage=pmax(y[obsMask], 0))
        } else {
            y[times > TDrop] = 0
            list(Time=times, Usage=pmax(y, 0))
        }
    }

    patNames = paste0('P', 1:patients)
    alldata = patCoefs[, do.call(genTs, .SD), by=.(Group, Id=factor(patNames, levels=patNames))] %>%
        setkey(Id, Time)

    # generate group trajectories
    groupTrajs = groupCoefs[, .(Time=times,
                                Usage=(pmax(Int + times * Slope + times^2 * Quad, 0) * AProb) %>% ifelse(times > TDrop, 0, .)
    ), by=Group]

    setattr(alldata, 'patCoefs', patCoefs)
    setattr(alldata, 'groupCoefs', groupCoefs)
    setattr(alldata, 'groupTrajs', groupTrajs)

    # Extra step: downsampling
    if(is.finite(nAggr)) {
        alldata = transformToAverage(alldata, binSize=nAggr)
    }
    return(alldata[])
}

transformToAverage = function(data, binSize=14) {
    bins = seq(min(data$Time), max(data$Time), by=binSize)
    nBins = length(bins) - 1
    bindata = data[, .(Usage=mean(Usage)), keyby=.(Group, Id, Bin=findInterval(Time, bins, all.inside=TRUE))] %>%
        .[, Time := bins[Bin]]

    groupTrajs = attr(data, 'groupTrajs')
    bingroupTrajs = groupTrajs[, .(Usage=mean(Usage)), keyby=.(Group, Bin=findInterval(Time, bins, all.inside=TRUE))] %>%
        .[, Time := bins[Bin]]

    setattr(bindata, 'groupTrajs', bingroupTrajs)
    return(bindata[])
}



transformToRepeatedMeasures = function(data) {
    assert_that(is.data.frame(data), has_name(data, c('Id', 'Time', 'Usage')))
    dtWide = dcast(data, Id ~ Time, value.var='Usage')

    dataMat = as.matrix(dtWide[, -'Id'])
    assert_that(nrow(dataMat) == uniqueN(data$Id), ncol(dataMat) == uniqueN(data$Time))
    rownames(dataMat) = dtWide$Id
    colnames(dataMat) = names(dtWide)[-1]
    return(dataMat)
}
