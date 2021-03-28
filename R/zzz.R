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
#' @section Getting started:
#' * See `vignette("demo", package = "latrend")` for an introduction to conducting a longitudinal cluster analysis on a example case study.
#' * See `vignette("custom", package = "latrend")` for examples on constructing your own cluster models.
#' * See `vignette("validation", package = "latrend")` for examples on applying internal cluster validation.
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
#' nCores = parallel::detectCores(logical = FALSE)
#' cl = parallel::makeCluster(nCores - 1)
#' parallel::clusterEvalQ(cl, expr=library(latrend))
#' }
#'
#' Then, register the cluster as the parallel back-end using the \link[doParallel]{doParallel-package}
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
#' In this example we use the \link[doMC]{doMC-package}
#' \preformatted{
#' nCores = parallel::detectCores(logical = FALSE)
#' doMC::registerDoMC(nCores - 1)
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
  'i', 'iseed', 'N', 'i.N', 'g', 'fun', 'method', 'plotTrajs',
  'Prob', 'Cluster', 'Class', 'Value', 'Id', 'Time',
  'Mu', 'Mu.cluster', 'Mu.class', 'Mu.fixed', 'Mu.random',
  'warning.Verbose',
  'TVEMMixNormal',
  '.Component'))
