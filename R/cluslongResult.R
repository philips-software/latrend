check_cluslongResult = function(object) {
    return(TRUE)
}

setClass('CluslongResult', slots=c(numClus='integer',
                                   clusters='factor',
                                   trends='data.table',
                                   criteria='numeric',
                                   start='POSIXct',
                                   runTime='numeric',
                                   converged='logical',
                                   model='ANY',
                                   details='list'),
         validity=check_cluslongResult)

#' @export
is.CluslongResult = function(object) {
    inherits(object, 'CluslongResult')
}

cluslongResult = function(clr, numClus, clusters, trends, start, runTime, criteria, converged, model=NULL, details=list()) {
    timeCol = clr@timeCol
    valueCol = clr@valueCol
    assert_that(is(clr, 'CluslongRecord'))
    assert_that(is.count(numClus))
    assert_that(is.numeric(clusters) || is.factor(clusters) || is.character(clusters))
    assert_that(length(clusters) == length(getIds(clr)), msg='cluster assignment vector length does not match the number of ids')
    assert_that(noNA(clusters), msg='cluster assignment contains NAs')
    assert_that(is.data.table(trends) && all(c('Cluster', timeCol, valueCol) %in% names(trends)))
    assert_that(is(start, 'POSIXct'))
    assert_that(is.numeric(runTime), is.na(runTime) || runTime >= 0)
    assert_that(is.numeric(criteria))

    setcolorder(trends, c('Cluster', timeCol, valueCol))
    setkeyv(trends, c('Cluster', timeCol))

    if(!is.factor(clusters) || nlevels(clusters) < numClus) {
        clusters = factor(clusters, levels=1:numClus)
    }

    new('CluslongResult', numClus=as.integer(numClus), clusters=clusters, trends=trends,
        criteria=criteria,
        start=start, runTime=runTime,
        converged=converged,
        model=model,
        details=details)
}

#' @export
#' @title Get the cluster names of the cluster result
getClusterNames = function(clResult) {
    levels(clResult@clusters)
}

#' @export
#' @title Get the cluster proportions
getClusterProps = function(clResult) {
    props = table(clResult@clusters) %>% prop.table
    x = as.numeric(props)
    names(x) = names(props)
    return(x)
}

#' @export
#' @title Get one or more computed criteria from the cluster result
#' @return The criterion value(s), or NA if the requested criterion does not exist. If criterion is missing or NULL, all criteria are returned.
getCriterion.CluslongResult = function(object, criterion=c()) {
    if(missing(criterion) || is.null(criterion)) {
        return(object@criteria)
    } else {
        assert_that(is.character(criterion))
        x = object@criteria[criterion]
        names(x) = criterion
        return(x)
    }
}
setMethod('getCriterion', signature=c('CluslongResult'), getCriterion.CluslongResult)

#' @export
#' @importFrom stats BIC
BIC.CluslongResult = function(x, ...) {
    getCriterion.CluslongResult(x, 'BIC')
}

#' @export
#' @import ggplot2
#' @title Plot trends of CluslongResult
plotTrends.CluslongResult = function(object, clusFormat='%s (%d%%)', lineSize=1.25) {
    clusNames = getClusterNames(object)
    clusProps = round(getClusterProps(object) * 100)
    xtrends = copy(object@trends)
    xtrends[, Cluster := factor(Cluster, levels=levels(Cluster), labels=sprintf(clusFormat, clusNames, clusProps))] %>%
        setnames(c('Cluster', 'Time', 'Value'))
    p = ggplot() +
        geom_line(data=xtrends, aes(x=Time, y=Value, color=Cluster), size=lineSize) +
        labs(x=get_trend_time(object@trends), y=get_trend_value(object@trends), title='Trends')

    return(p)
}
setMethod('plotTrends', signature=c('CluslongResult'), plotTrends.CluslongResult)

setMethod('plot', signature=c('CluslongResult'), function(x, ...) {
    plotTrends(x, ...)
})