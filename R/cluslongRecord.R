check_cluslongRecord = function(object) {
    return(TRUE)
}

setClass('CluslongRecord', slots=c(name='character',
                                   idCol='character',
                                   timeCol='character',
                                   valueCol='character',
                                   data='data.table',
                                   results='list'), validity=check_cluslongRecord)

#' @export
setGeneric('getCriterion', function(object, criterion) standardGeneric('getCriterion'))
#' @export
setGeneric('plotTrends', function(object, ...) standardGeneric('plotTrends'))


#' @export
is.CluslongRecord = function(object) {
    is(object, 'CluslongRecord')
}

#' @export
#' @title Create data object for clustering longitudinally
#' @param data A \code{data.frame} or \code{data.table} containing the longitudinal data in long format, with an observation per row.
#' @param idCol Name of the column indicating the trajectory to which the observation belongs. If missing, and \code{data} is keyed, the first key is taken as the ID column. If not keyed, the first column is used.
#' @param timeCol Name of the column containing the observation times. If missing, and \code{data} is keyed, the second key is used. If not keyed, the second column is used.
#' @param valueCol Name of the column containing the observed values. If missing, and \code{data} is keyed, the first non-key column is used. If not keyed, the third column is used.
#' @param name Description of the object, primarily used for saving to a file.
cluslongRecord = function(data, idCol, timeCol, valueCol, name='clusLongRecord') {
    assert_that(is.data.frame(data), msg='data should be a data.frame or data.table')
    assert_that(length(data) >= 3, msg='data should contain 3 columns (id, time, value)')

    if(missing(idCol) || is.null(idCol)) {
        idCol = get_id(data)
    } else {
        assert_that(is.scalar(idCol), has_name(data, idCol))
    }
    if(missing(timeCol) || is.null(timeCol)) {
        timeCol = get_time(data)
    } else {
        assert_that(is.scalar(timeCol), has_name(data, timeCol))
    }
    if(missing(valueCol) || is.null(valueCol)) {
        valueCol = get_value(data)
    } else {
        assert_that(is.scalar(valueCol), has_name(data, valueCol))
    }

    if(!is.data.table(data)) {
        data = data.table(data, key=c(idCol, timeCol))
    }

    new('CluslongRecord', name=name, idCol=idCol, timeCol=timeCol, valueCol=valueCol, data=data)
}

#' @export
#' @title Get the time series Ids of a CluslongRecord object
#' @return The IDs in the dataset
getIds = function(clr) {
    x = clr@data[[clr@idCol]]
    if(is.factor(x)) {
        levels(x)
    } else {
        unique(x)
    }
}

#' @export
#' @title Get the observation times of a CluslongRecord object
#' @return The unique observation times in the dataset
getTimes = function(clr) {
    unique(clr@data[[clr@timeCol]])
}

#' @export
#' @title Get the results for the given number of clusters
#' @param numClus Vector of number of clusters. If missing, returns all results
#' @param drop Whether to drop the list in case 1 result is returned
#' @return A list of CluslongResult objects
getResults = function(clr, numClus, drop=TRUE) {
    assert_that(is.flag(drop))
    if(missing(numClus)) {
        resultNames = names(clr@results)
    } else {
        assert_that(all(is.finite(numClus)), msg='NA/infinite values supplied')
        assert_that(all(numClus > 0 & numClus %% 1 == 0), msg='numClus must be a positive integer')
        resultNames = paste0('c', numClus)
        assert_that(all(resultNames %in% names(clr@results)),
                    msg=paste0('no results for ', paste(setdiff(resultNames, names(clr@results)), collapse=', ')))

    }

    if(drop && length(resultNames) == 1) {
        clr@results[[resultNames]]
    } else {
        clr@results[resultNames]
    }
}

#' @export
#' @title Remove the results from a CluslongRecord
clearResults = function(clr) {
    clrName = deparse(substitute(clr))
    clr@results = list()
    assign(clrName, clr, envir=parent.frame())
}

#' @export
#' @title Get one or more criteria across the computed results
#' @param criterion Name(s) of criteria to look up. If missing or NULL, all criteria are returned.
#' @return A data.frame with a column per criterion, and the first column indicating the number of clusters
getCriterion.CluslongRecord = function(object, criterion=c()) {
    assert_that(not_empty(object@results), msg='no results')
    if(missing(criterion) || is.null(criterion)) {
        criterion = names(object@results[[1]]@criteria)
    } else {
        assert_that(is.character(criterion))
    }
    do.call(rbind, lapply(object@results, function(r) c(numClus=r@numClus, r@criteria[criterion]))) %>%
        as.data.frame
}
setMethod('getCriterion', signature=c('CluslongRecord'), getCriterion.CluslongRecord)


#' @export
#' @title Plot a criterion over the evaluated results
#' @param criterion Name(s) of the criteria to plot
#' @param normalize Whether to rescale the criterion values between [0, 1]
plotCriterion = function(x, criterion=c(), normalize=FALSE, lineSize=1, dotSize=2) {
    dt_crit = getCriterion.CluslongRecord(x, criterion) %>%
        as.data.table %>%
        melt(id='numClus', variable.name='Criterion', value.name='Value')

    if(normalize) {
        dt_crit[, Value := (Value - min(Value, na.rm=TRUE)) / (max(Value, na.rm=TRUE) - min(Value, na.rm=TRUE)), by=Criterion]
    }

    if(length(criterion) == 1) {
        p = ggplot(dt_crit, aes(x=numClus, y=Value)) +
            ggtitle(criterion)
    } else {
        p = ggplot(dt_crit, aes(x=numClus, y=Value, color=Criterion, shape=Criterion)) +
            ggtitle('Cluster criteria')
    }

    panelColor = theme_get()$panel.background$fill

    p = p + geom_line() +
        geom_line(size=lineSize)

    if(!is.null(panelColor)) {
        p = p + geom_point(color=panelColor, size=dotSize*2)
    }

    p = p + geom_point(size=dotSize) +
        labs(x='Number of clusters', y=ifelse(normalize, 'Normalized value', 'Value'))
    return(p)
}

#' @export
#' @title Plot trajectories of a CluslongRecord
#' @param sample Number of time series to sample per cluster for plotting
plotTrajectories = function(clr, sample=Inf, tsColor='black', tsSize=.1, tsAlpha=1) {
    assert_that(is.scalar(sample), sample > 0)

    xdata = copy(clr@data) %>%
        setnames(c(clr@idCol, clr@timeCol, clr@valueCol), c('Id', 'Time', 'Value'))

    if(sample < Inf) {
        dt_ids = data.table(Id=getIds(clr)) %>%
            .[base::sample(1:.N, min(sample, .N))]
        xdata = xdata[dt_ids]
    }

    p = ggplot() +
        geom_line(data=xdata, aes(x=Time, y=Value, group=Id), color=tsColor, size=tsSize, alpha=tsAlpha) +
        labs(title='Trajectories',
             x=clr@timeCol,
             y=clr@valueCol)

    return(p)
}

#' @export
setMethod('plot', signature=c('CluslongRecord'), function(x, ...) {
    plotTrajectories(x, ...)
})

#' @export
#' @title Plot trends of a CluslongResult with the trajectories from the CluslongRecord
#' @param numClus Number of clusters
#' @param ribbon Whether to show the within-group variability using a ribbon, instead of plotting individual time series
#' @param ribbonQ Quantile range of the ribbon (90pct interval by default)
#' @param sample Number of time series to sample per cluster for plotting. Only applicable when ribbon=FALSE.
#' @param nrow Number of rows for the facet
#' @param ncol Number of columns for the facet
plotTrends.CluslongRecord = function(object, numClus,
                                     ribbon=FALSE,
                                     ribbonQ=c(.05, .95),
                                     sample=Inf,
                                     tsColor='steelblue1',
                                     tsSize=.1,
                                     tsAlpha=.5,
                                     trendColor='black',
                                     trendSize=2,
                                     nrow=NULL,
                                     ncol=NULL,
                                     clusFormat='%s (%d%%)') {
    assert_that(is.count(numClus))
    assert_that(is.number(sample), sample > 0)
    assert_that(is.numeric(ribbonQ), length(ribbonQ) == 2)
    clResult = getResults(object, numClus)

    # Prepare data
    clusNames = getClusterNames(clResult)
    clusProps = round(getClusterProps(clResult) * 100)
    newClusNames = sprintf(clusFormat, clusNames, clusProps)
    newClusters = factor(clResult@clusters, levels=clusNames, labels=newClusNames)

    xtrends = copy(clResult@trends)
    xtrends[, Cluster := factor(Cluster, levels=levels(Cluster), labels=newClusNames)] %>%
        setnames(c('Cluster', 'Time', 'Value'))
    xdata = copy(object@data) %>%
        setnames(c(object@idCol, object@timeCol, object@valueCol), c('Id', 'Time', 'Value'))
    xdata[, Cluster := newClusters[.GRP], by=Id]

    if(ribbon) {
        p = plotData_ribbon(xdata, q=ribbonQ, tsColor=tsColor, tsAlpha=tsAlpha)
    } else {
        p = plotData_line(xdata, nsample=sample, tsColor=tsColor, tsSize=tsSize, tsAlpha=tsAlpha)
    }

    p + geom_line(data=xtrends, aes(x=Time, y=Value), color=trendColor, size=trendSize) +
        facet_wrap(~ Cluster, ncol=ncol, nrow=nrow) +
        labs(title='Trends',
             x=get_trend_time(clResult@trends),
             y=get_trend_value(clResult@trends))
}
setMethod('plotTrends', signature=c('CluslongRecord'), plotTrends.CluslongRecord)

plotData_line = function(xdata, nsample, tsColor, tsSize, tsAlpha) {
    if(nsample < Inf) {
        dt_assign = xdata[, .(Cluster=first(Cluster)), by=Id] %>%
            .[, .(Id=sample(Id, min(nsample, .N))), by=Cluster]
        xdata = xdata[dt_assign[, 'Id']]
    }

    p = ggplot() +
        geom_line(data=xdata, aes(x=Time, y=Value, group=Id), color=tsColor, size=tsSize, alpha=tsAlpha)
}

plotData_ribbon = function(xdata, q, tsColor, tsAlpha) {
    dt_ribbon = xdata[, as.list(quantile(Value, q, na.rm=TRUE)), by=.(Cluster, Time)] %>%
        setnames(c('Cluster', 'Time', 'ymin', 'ymax'))

    p = ggplot() +
        geom_ribbon(data=dt_ribbon, aes(x=Time, ymin=ymin, ymax=ymax), fill=tsColor, alpha=tsAlpha)
}
