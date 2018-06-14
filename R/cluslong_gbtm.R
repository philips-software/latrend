#' @export
#' @import lcmm
#' @title Group-based trajectory modeling
#' @seealso \link{cluslong_gmm}
#' @inheritParams cluslong
#' @inheritParams cluslong_gmm
#' @param start Model initialization method. Either gridsearch or kml
#' @param startMaxIter Number of model optimization iterations for the initialization method
cluslong_gbtm = function(data,
                        numClus=2:3,
                        maxIter=NULL,
                        numRuns=25,
                        fixed,
                        mixture,
                        start='gridsearch',
                        startMaxIter=ifelse(start[1] == 'gridsearch', 20, 0),
                        idCol,
                        timeCol,
                        valueCol,
                        resultFun=NULL,
                        keep=getOption('cluslong.keep', 'all'),
                        verbose=TRUE,
                        seed=NULL) {
    do.call(cluster_longitudinal, c(mget(names(formals()), sys.frame(sys.nframe())),
                                    prepFun=prep_gmm,
                                    clusterFun=cluster_gmm,
                                    random=~-1,
                                    diagCov=TRUE,
                                    classCov=FALSE))
}
