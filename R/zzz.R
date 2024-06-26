#' @import data.table
#' @import assertthat
#' @import methods
#' @import magrittr
#' @import stats
#' @importFrom Rdpack reprompt
#' @importFrom foreach foreach %do% %dopar%
#' @importFrom utils hasName capture.output combn getS3method modifyList head tail data
# just present to get rid of the "Namespace in Imports field not imported from: 'rmarkdown'" note in R CMD check
# the note is incorrect because putting rmarkdown in Suggets field results in vignettes failing to build
#' @importFrom rmarkdown html_vignette
#' @aliases latrend-package
#' @rdname latrend-package
#' @section Features:
#' * **Unified cluster analysis**, independent of the underlying algorithms used. Enabling users to compare the performance of various longitudinal cluster methods on the case study at hand.
#' * Supports [many different methods][latrend-methods] for longitudinal clustering out of the box (see the list of supported packages below).
#' * The framework consists of extensible S4 methods based on an abstract [model class][lcModel-class], enabling **rapid prototyping** of new cluster methods or model specifications.
#' * Standard **plotting** tools for model evaluation across methods (e.g., [trajectories][trajectories], [cluster trajectories][clusterTrajectories], model fit, [metrics][latrend-metrics])
#' * Support for many **[cluster metrics][latrend-metrics]** through the packages *clusterCrit*, *mclustcomp*, and *igraph*.
#' * The structured and unified analysis approach enables simulation studies for **comparing methods**.
#' * Standardized model validation for all methods through [bootstrapping][latrendBoot] or [k-fold cross-validation][latrendCV].
#'
#' The supported types of longitudinal datasets are described [here][latrend-data].
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
#' We can then investigate the fitted model using
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
#' * See `vignette("simulation", package = "latrend")` for an example on conducting a simulation study.
#' * See `vignette("validation", package = "latrend")` for examples on applying internal cluster validation.
#' * See `vignette("implement", package = "latrend")` for examples on constructing your own cluster models.
#' @section Useful pages:
#' Data requirements and datasets:
#' [latrend-data] [latrendData] [PAP.adh]
#'
#' High-level method recommendations and supported methods:
#' [latrend-approaches] [latrend-methods]
#'
#' Method specification:
#' [lcMethod-class][lcMethod-class]
#' [lcMethods]
#'
#' Method estimation:
#' [latrend] [latrendRep] [latrendBatch] [latrendBoot] [latrendCV]
#' [latrend-parallel]
#' [Steps performed during estimation][lcMethod-estimation]
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
#' @title Parallel computation using latrend
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
#' \donttest{
#' data(latrendData)
#'
#' # parallel latrendRep()
#' method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
#' models <- latrendRep(method, data = latrendData, .rep = 5, parallel = TRUE)
#'
#' # parallel latrendBatch()
#' methods <- lcMethods(method, nClusters = 1:3)
#' models <- latrendBatch(methods, data = latrendData, parallel = TRUE)
#' }
#' @seealso [latrendRep], [latrendBatch], [latrendBoot], [latrendCV]
NULL

.onLoad = function(libname, pkgname) {
  opts = list(
    latrend.verbose = R.utils::Verbose(threshold = -1),
    latrend.time = 'Time',
    latrend.id = 'Id',
    latrend.clusterNames = LETTERS,
    latrend.printOptions = FALSE,
    latrend.printSharedModelArgs = FALSE,
    latrend.warnModelDataClusterColumn = TRUE,
    latrend.warnNewDataClusterColumn = TRUE,
    latrend.warnTrajectoryLength = 1
  )

  optMask = !(names(opts) %in% names(options()))
  if (any(optMask)) {
    options(opts[optMask])
  }
}

.loadOptionalPackage = function(name) {
  assert_that(is.string(name))

  .checkPackageInstalled(name)

  pkgEnvName = rlang::pkg_env_name(name)

  if (!rlang::is_attached(pkgEnvName)) {
    if (requireNamespace(name, quietly = TRUE)) {
      ns = loadNamespace(name)
      attachNamespace(ns)
    } else {
      # fallback
      stop('unable to load required package "', name , '". Install the package to use this method.')
    }
  }
}

.checkPackageInstalled = function(name) {
  assert_that(is.string(name))

  if (!rlang::is_installed(name)) {
    stop(
      sprintf(
        'The "%1$s" package is required. Install the package to use this method.\n\tRun: install.packages("%1$s")',
        name
      )
    )
  }
}

# temporary work-around for aes_string/aes replacement
.as_lang = function(x) {
  if (is.language(x)) {
    x
  } else if (is.character(x)) {
    as.symbol(x)
  } else {
    stop('cannot cast to language')
  }
}


globalVariables(
  c(
    '.', '.id', '.time', '.resp', '..id', '..time', '..response',
    '.Component', '.Fill', '.name', '.group', '.method', '.ROW_INDEX', '.Mean', '.Block',
    'akclustr', 'AllNA',
    'Cluster', 'Class', 'cl',
    'i', 'i.N', 'iseed', 'Include', 'Id',
    'k',
    'g', 'Group',
    'fun',
    'method', 'modelMethod', 'modelData', 'modelCall', 'Mu', 'Mu.cluster', 'Mu.class', 'Mu.fixed', 'Mu.random',
    'N',
    'plotTrajs', 'Prob',
    'Time', 'TVEMMixNormal',
    'Value',
    'warning.Verbose', 'weighted.mean',
    'ymin', 'ymax'
  )
)
