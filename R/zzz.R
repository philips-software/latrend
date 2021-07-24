#' @import data.table
#' @import assertthat
#' @import methods
#' @import magrittr
#' @import stats
#' @importFrom Rdpack reprompt
#' @importFrom scales percent
#' @importFrom foreach foreach %do% %dopar%
#' @import ggplot2
#' @importFrom utils hasName capture.output combn getS3method modifyList head tail data
#' @aliases latrend-package
#' @rdname latrend-package
#' @section Features:
#' * Unified cluster analysis, independent of the underlying algorithms used. Enabling users to compare the performance of various longitudinal cluster methods on the case study at hand.
#' * Supports many different methods for longitudinal clustering out of the box (see the list of supported packages below).
#' * The framework consists of extensible S4 methods based on an abstract model class, enabling rapid prototyping of new cluster methods or model specifications.
#' * Standard plotting tools for model evaluation across methods (e.g., trajectories, cluster trajectories, model fit, metrics)
#' * Support for many cluster metrics through the packages [clusterCrit][clusterCrit-package], [mclustcomp][mclustcomp-package], and igraph.
#' * The structured and unified analysis approach enables simulation studies for comparing methods.
#' * Standardized model validation for all methods through bootstrapping or k-fold cross-validation.
#'
#' @section Getting started:
#' The [latrendData] dataset is included with the package and is used in all examples.
#' The [plotTrajectories()] function can be used to visualize any longitudinal dataset, given the `id` and `time` are specified.
#' \preformatted{
#' data(latrendData)
#' head(latrendData)
#' options(latrend.id = "Id", latrend.time = "Time")
#' plotTrajectories(latrendData, response = "Y")
#' }
#'
#' Discovering longitudinal clusters using the package involves the specification of the longitudinal cluster method that should be used.
#' \preformatted{
#' kmlMethod <- lcMethodKML("Y", nClusters = 3)
#' kmlMethod
#' }
#'
#' The specified method is then estimated on the data using the generic estimation procedure function [latrend()]:
#' \preformatted{
#' model <- latrend(kmlMethod, data = latrendData)
#' }
#'
#' Analyze the fitted model
#' \preformatted{
#' summary(model)
#' plot(model)
#' metric(model, c("WMAE", "BIC"))
#' qqPlot(model)
#' }
#'
#' Create derivative method specifications for 1 to 5 clusters using the [lcMethods()] function.
#' A series of methods can be estimated using [latrendBatch()].
#' \preformatted{
#' kmlMethods <- lcMethods(kmlMethod, nClusters = 1:5)
#' models <- latrendBatch(kmlMethods, data = latrendData)
#' }
#'
#' Determine the number of clusters through one or more internal cluser metrics.
#' This can be done visually using the [plotMetric()] function.
#' \preformatted{
#' plotMetric(models, c("WMAE", "BIC"))
#' }
#'
#' @section Vignettes:
#' Further step-by-step instructions on how to use the package are described in the vignettes.
#' * See `vignette("demo", package = "latrend")` for an introduction to conducting a longitudinal cluster analysis on a example case study.
#' * See `vignette("custom", package = "latrend")` for examples on constructing your own cluster models.
#' * See `vignette("validation", package = "latrend")` for examples on applying internal cluster validation.
#' @section Useful pages:
#' Method specification:
#' [lcMethod-class][lcMethod-class]
#' [lcMethods]
#'
#' Method estimation:
#' [latrend] [latrendRep] [latrendBatch] [latrendBoot] [latrendCV]
#' [latrend-parallel]
#'
#' Model functions:
#' [lcModel-class][lcModel-class]
#' [clusterTrajectories] [plotClusterTrajectories]
#' [postprob] [trajectoryAssignments] [predictPostprob] [predictAssignments]
#' [predict.lcModel] [predictForCluster] [fitted.lcModel] [fittedTrajectories]
"_PACKAGE"

#' @name latrend-generics
#' @rdname latrend-generics
#' @title Generics used by latrend for different classes
NULL

#' @name latrend-parallel
#' @rdname latrend-parallel
#' @title Parallel computing using latrend
#' @description The model estimation functions support parallel computation through the use of the \link[foreach]{foreach} mechanism.
#' In order to make use of parallel execution, a parallel back-end must be registered.
#'
#' @section Windows:
#' On Windows, the \link[parallel]{parallel-package} can be used to define parallel socket workers.
#' \preformatted{
#' nCores <- parallel::detectCores(logical = FALSE)
#' cl <- parallel::makeCluster(nCores)
#' }
#'
#' Then, register the cluster as the parallel back-end using the `doParallel` package:
#' \preformatted{
#' doParallel::registerDoParallel(cl)
#' }
#'
#' If you defined your own `lcMethod` or `lcModel` extension classes, make sure to load them on the workers as well.
#' This can be done, for example, using:
#' \preformatted{
#' parallel::clusterEvalQ(cl,
#'   expr = setClass('lcMethodMyImpl', contains = "lcMethod"))
#' }
#'
#' @section Unix:
#' On Unix systems, it is easier to setup parallelization as the R process is forked.
#' In this example we use the `doMC` package:
#' \preformatted{
#' nCores <- parallel::detectCores(logical = FALSE)
#' doMC::registerDoMC(nCores)
#' }
#'
#' @examples
#' data(latrendData)
#'
#' # parallel latrendRep()
#' method <- lcMethodKML(response = "Y")
#' models <- latrendRep(method, data = latrendData, .rep = 10, parallel = TRUE)
#'
#' # parallel latrendBatch()
#' methods <- lcMethods(method, nClusters = 1:3)
#' models <- latrendBatch(methods, data = latrendData, parallel = TRUE)
#' @seealso [latrendRep], [latrendBatch], [latrendBoot], [latrendCV]
NULL

.onLoad = function(libname, pkgname) {
  opts = list(
    latrend.verbose = R.utils::Verbose(threshold = -1),
    latrend.time = 'Time',
    latrend.id = 'Id',
    latrend.clusterNames = LETTERS,
    latrend.printOptions = FALSE,
    latrend.printSharedModelArgs = FALSE
  )

  optMask = !(names(opts) %in% names(options()))
  if (any(optMask)) {
    options(opts[optMask])
  }
}

.loadOptionalPackage = function(name) {
  if(not(name %in% .packages())) {
    if(requireNamespace(name, quietly = TRUE)) {
      ns = loadNamespace(name)
      attachNamespace(ns)
    } else {
      stop('unable to load required package "', name , '". Install the package to use this method.')
    }
  }
}

globalVariables(c('.', '.name', '.group', '.method', '.ROW_INDEX', '.Mean', '.Block',
  'i', 'iseed', 'N', 'i.N', 'g', 'fun', 'method', 'plotTrajs', 'cl',
  'Prob', 'Cluster', 'Class', 'Value', 'Id', 'Time',
  'Mu', 'Mu.cluster', 'Mu.class', 'Mu.fixed', 'Mu.random',
  'warning.Verbose',
  'TVEMMixNormal',
  '.Component'))
