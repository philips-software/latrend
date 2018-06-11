#' @export
#' @import kml
#' @import lcmm
#' @title Two-step clustering using growth curve modeling and k-means
#' @inheritParams cluslong
#' @inheritParams cluslong_twostep
#' @param gcmFixed Fixed effects of the mixed model
#' @param gcmRandom Random effects of the mixed model
#' @param gcmDiagCov Whether a diagonal variance-covariance matrix should be used (see idiag in lcmm::hlme)
#' @param gcmMaxIter Number of maximum iterations for the mixed model
cluslong_gckm = function(data,
                         numClus=1:4,
                         numRuns=10,
                         maxIter=500,
                         gcmFixed=Value ~ Time,
                         gcmRandom=Value ~ Time,
                         gcmDiagCov=TRUE,
                         gcmMaxIter=maxIter,
                         standardize=scale,
                         kmStart='kmeans++',
                         kmImputation='copyMean',
                         kmDistance='euclidean',
                         kmCenter=meanNA,
                         kmMaxIter=maxIter,
                         idCol,
                         timeCol,
                         valueCol,
                         keep=getOption('cluslong.keep', 'all'),
                         verbose=TRUE) {
    do.call(cluslong, c(method='gckm', mget(names(formals()), sys.frame(sys.nframe()))))
}



cl_gckm = function(clr, updateFun, numClus, numRuns, maxIter,
                   gcmFixed, gcmRandom, gcmDiagCov, gcmMaxIter,
                   standardize,
                   kmStart, kmImputation, kmDistance, kmCenter, kmMaxIter,
                   keep, verbose) {

    representFun = function(clr) {
        gcm = eval(substitute(hlme(fixed=gcmFixed, random=gcmRandom, subject=clr@idCol, ng=1,
                                   idiag=gcmDiagCov, maxiter=gcmMaxIter, data=clr@data, verbose=verbose)))
        R = ranef(gcm)
        return(list(coefs=R,
                    preds=gcm$pred$pred_ss,
                    converged=gcm$conv == 1,
                    model=gcm))
    }

    clusterFun = function(X, g) {
        cld = clusterLongData(traj=X, idAll=rownames(X), time=seq_len(ncol(X)), varNames='Value')
        par = parALGO(saveFreq=1e99, scale=FALSE, maxIt=kmMaxIter,
                      startingCond=kmStart, imputationMethod=kmImputation, distanceName=kmDistance, centerMethod=kmCenter)
        kml(cld, nbClusters=g, nbRedrawing=numRuns, toPlot='none', parAlgo=par)

        model = slot(cld, paste0('c', g))[[1]]
        criteria = model@criterionValues
        criteria['BIC'] = -criteria['BIC']
        criteria['AIC'] = -criteria['AIC']

        return(list(clusters=getClusters(cld, g),
                    criteria=criteria,
                    converged=TRUE,
                    model=model))
    }

    trendFun = function(clr, clusters, preds, ...) {
        pred_traj = data.table(clr@data[, .(Id=get(clr@idCol), Time=get(clr@timeCol))], Pred=preds)
        rowClusters = rep(clusters, clr@data[, .N, by=c(clr@idCol)]$N)
        pred_traj[, Cluster := rowClusters]
        dt_trends = pred_traj[, .(Value=mean(Pred, na.rm=TRUE)), by=.(Cluster, Time)] %>%
            setnames('Time', clr@timeCol)
        return(dt_trends)
    }

    cl_twostep(clr, updateFun, numClus, numRuns, maxIter,
               representStep=representFun, standardize=standardize, clusterStep=clusterFun, trendEstimator=trendFun,
               verbose=verbose)
}

