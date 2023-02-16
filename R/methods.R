#' @include method.R

setOldClass('lcMethods')


#' @name latrend-methods
#' @title Supported methods for longitudinal clustering
#' @description This page provides an overview of the currently supported methods for longitudinal clustering.
#' For general recommendations on which method to apply to your dataset, [see here][latrend-approaches].
#' @section Supported methods:
#' | **Method** | **Description** | **Source** |
#' | --- | :-------- | :--- |
#' | [lcMethodAkmedoids] | Anchored *k*-medoids \insertCite{adepeju2020akmedoids}{latrend} | `akmedoids` |
#' | [lcMethodCrimCV] | Group-based trajectory modeling of count data \insertCite{nielsen2018crimcv}{latrend} | `crimCV` |
#' | [lcMethodDtwclust] | Methods for distance-based clustering, including dynamic time warping \insertCite{sardaespinosa2019time}{latrend} | `dtwclust` |
#' | [lcMethodFeature] | Feature-based clustering | |
#' | [lcMethodFlexmix] | Interface to the FlexMix framework \insertCite{gruen2008flexmix}{latrend} | `flexmix` |
#' | [lcMethodFlexmixGBTM] | Group-based trajectory modeling | `flexmix` |
#' | [lcMethodFunFEM] | Model-based clustering using funFEM \insertCite{bouveyron2015funfem}{latrend} | `funFEM` |
#' | [lcMethodGCKM] | Growth-curve modeling and *k*-means | `lme4` |
#' | [lcMethodKML] | Longitudinal *k*-means \insertCite{genolini2015kml}{latrend} | `kml` |
#' | [lcMethodLcmmGBTM] | Group-based trajectory modeling \insertCite{proustlima2017estimation}{latrend} | `lcmm` |
#' | [lcMethodLcmmGMM] | Growth mixture modeling \insertCite{proustlima2017estimation}{latrend} | `lcmm` |
#' | [lcMethodLMKM] | Feature-based clustering using linear regression and *k*-means | |
#' | [lcMethodMclustLLPA] | Longitudinal latent profile analysis \insertCite{scrucca2016mclust}{latrend} | `mclust` |
#' | [lcMethodMixAK_GLMM] | Mixture of generalized linear mixed models | `mixAK` |
#' | [lcMethodMixtoolsGMM] | Growth mixture modeling | `mixtools` |
#' | [lcMethodMixtoolsNPRM] | Non-parametric repeated measures clustering \insertCite{benaglia2009mixtools}{latrend} | `mixtools` |
#' | [lcMethodMixTVEM] | Mixture of time-varying effects models | |
#' | [lcMethodRandom] | Random partitioning | |
#' | [lcMethodStratify] | Stratification rule | |
#'
#' In addition, the functionality of any method can be extended via [meta methods][lcMetaMethods].
#' This is used for extending the estimation procedure of a method, such as [repeated fitting][lcFitRep] and selecting the best result, or [fitting until convergence][lcFitConverged].
#'
#' It is strongly encouraged to [evaluate and compare][latrend-metrics] several candidate methods in order to identify the most suitable method.
#' @references \insertAllCited{}
#' @seealso [latrend-approaches] [latrend-estimation] [latrend-metrics]
#' @examples
#' data(latrendData)
#' method <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
#' model <- latrend(method, data = latrendData)
NULL

#' @name latrend-approaches
#' @title High-level approaches to longitudinal clustering
#' @description This page provides high-level guidelines on which methods are applicable to your dataset.
#' Note that this is intended as a quick-start.
#'
#' Recommended overview and comparison papers:
#' \itemize{
#'   \item \insertCite{denteuling2021clustering}{latrend}: A tutorial and overview on methods for longitudinal clustering.
#'   \item \insertCite{denteuling2021comparison;textual}{latrend} compared KmL, MixTVEM, GBTM, GMM, and GCKM.
#'   \item \insertCite{twisk2012classifying;textual}{latrend} compared KmL, GCKM, LLCA, GBTM and GMM.
#'   \item \insertCite{verboon2022clustering;textual}{latrend} compared the *kml*, *traj* and *lcmm* packages in R.
#'   \item \insertCite{martin2015growth;textual}{latrend} compared KmL, LCA, and GMM.
#' }
#'
#' @section Approaches:
#' Disclaimer: The table below has been adapted from a pre-print of \insertCite{denteuling2021clustering}{latrend}.
#'
#' | **Approach** | **Strengths** | **Limitations** | **Methods** |
#' | --- | :-------- | :--- | :--- |
#' | **Cross-sectional clustering** | \itemize{\item Suitable for large datasets \item Many available algorithms \item Non-parametric cluster trajectory representation} | \itemize{\item Requires time-aligned complete data \item Sensitive to measurement noise} | [lcMethodKML] [lcMethodMclustLLPA] [lcMethodMixtoolsNPRM] |
#' | **Distance-based clustering** | \itemize{\item Suitable for medium-sized datasets \item Many possible distance metrics \item Distance matrix only needs to be computed once} | \itemize{\item Scales poorly with number of trajectories \item No robust cluster trajectory representation \item Some distance metrics require aligned observations} | [lcMethodDtwclust] |
#' | **Feature-based clustering** | \itemize{\item Suitable for large datasets \item Configurable \item Features only needs to be computed once \item Compact trajectory representation} | \itemize{\item Generally requires intensive longitudinal data \item Sensitive to outliers} | [lcMethodFeature] [lcMethodAkmedoids] [lcMethodLMKM] [lcMethodGCKM] |
#' | **Model-based clustering** | \itemize{\item Parametric cluster trajectory \item Incorporate (domain) assumptions \item Low sample size requirements} | \itemize{\item Computationally intensive \item Scales poorly with number of clusters \item Convergence challenges} | [lcMethodLcmmGBTM] [lcMethodLcmmGMM] [lcMethodCrimCV] [lcMethodFlexmix] [lcMethodFlexmixGBTM] [lcMethodFunFEM] [lcMethodMixAK_GLMM] [lcMethodMixtoolsGMM] [lcMethodMixTVEM] |
#'
#' It is strongly encouraged to [evaluate and compare][latrend-metrics] several candidate methods in order to identify the most suitable method.
#' @references \insertAllCited{}
#' @seealso [latrend-methods] [latrend-estimation] [latrend-metrics]
NULL


#' @export
#' @title Convert a list of lcMethod objects to a data.frame
#' @description Converts a list of `lcMethod` objects to a `data.frame`.
#' @inheritParams as.data.frame.lcMethod
#' @param x the `lcMethods` or `list` to be coerced to a `data.frame`.
#' @param ... Additional arguments.
#' @return A `data.frame` with each row containing the argument values of a method object.
#' @family lcMethod functions
as.data.frame.lcMethods = function(x, ..., eval = TRUE, nullValue = NA, envir = parent.frame()) {
  df = lapply(x, as.data.frame, ..., eval = eval, nullValue = nullValue, envir = envir) %>%
    rbindlist(fill = TRUE) %>%
    as.data.frame()

  cbind(.class = vapply(x, class, FUN.VALUE = ''), df)
}

#' @export
#' @title Convert a list of lcMethod objects to a lcMethods list
#' @param x A `list` of `lcMethod` objects.
#' @return A `lcMethods` object.
#' @family lcMethod functions
as.lcMethods = function(x) {
  if (!is.list(x)) {
    x = list(x)
  }

  class(x) = c('lcMethods', 'list')
  x
}

#' @export
#' @title Generate a list of lcMethod objects
#' @description Generates a list of `lcMethod` objects for all combinations of the provided argument values.
#' @param method The `lcMethod` to use as the template, which will be updated for each of the other arguments.
#' @param ... Any other arguments to update the `lcMethod` definition with. Values must be `scalar`, `vector`, `list`, or encapsulated in a `.()` call.
#' Arguments wrapped in `.()` are passed as-is to the model call, ensuring a readable method.
#' Arguments comprising a single `symbol` (e.g. a variable name) are interpreted as a constant. To force evaluation, specify `arg=(var)` or `arg=force(var)`.
#' Arguments of type `vector` or `list` are split across a series of method fit calls.
#' Arguments of type `scalar` are constant across the method fits.
#' If a `list` is intended to be passed as a constant argument, then specifying `arg=.(listObject)` results in it being treated as such.
#' @param envir The `environment` in which to evaluate the method arguments.
#' @return A `list` of `lcMethod` objects.
#' @examples
#' data(latrendData)
#' baseMethod <- lcMethodLMKM(Y ~ Time, id = "Id", time = "Time")
#' methods <- lcMethods(baseMethod, nClusters = 1:6)
#'
#' nclus <- 1:6
#' methods <- lcMethods(baseMethod, nClusters = nclus)
#'
#' # list notation, useful for providing functions
#' methods <- lcMethods(baseMethod, nClusters = .(1, 3, 5))
#' length(methods) # 3
lcMethods = function(method, ..., envir = NULL) {
  assert_that(is.lcMethod(method))

  envir = .selectEnvironment(method, parent.frame(), envir)

  mc = match.call()[-1]
  assert_that(not('' %in% names(mc)), msg = 'method arguments must be named')
  argNames = names(mc) %>% setdiff(c('method', 'envir'))
  argCalls = as.list(mc)[argNames]

  nameMask = vapply(argCalls, is.name, FUN.VALUE = FALSE)
  dotMask = vapply(argCalls, function(x) is.call(x) && x[[1]] == '.', FUN.VALUE = FALSE)
  evalMask = !nameMask & !dotMask

  nameArgs = lapply(
    which(nameMask),
    function(i) {
      tryCatch({
          argVal = eval(argCalls[[i]], envir = envir)
          as.list(argVal)
        },
        error = function(...) argCalls[i]
      )
    }
  )
  dotArgs = lapply(which(dotMask), function(i) as.list(argCalls[[i]][-1]))

  evalArgs = lapply(
    which(evalMask),
    function(i) {
      tryCatch({
          eval(argCalls[[i]], envir = envir)
        },
        error = function(e, ...) {
          stop(sprintf(
            'Error occurred while evaluating lcMethods argument "%s":\n  "%s"\n',
            argNames[i],
            e$message
          ))
        }
      )
    }
  )

  assert_that(
    all(vapply(nameArgs, is.list, FUN.VALUE = FALSE)),
    all(vapply(dotArgs, is.list, FUN.VALUE = FALSE)),
    all(vapply(evalArgs, is.vector, FUN.VALUE = FALSE)),
    msg = 'The processed argument lists are in an unexpected format. Please report this issue.'
  )

  # combine the different arguments into a name-sorted list
  allArgs = c(nameArgs, dotArgs, evalArgs)
  allArgs = allArgs[sort(names(allArgs))]
  argNs = lengths(allArgs)

  # generate combinations
  combIdx = lapply(argNs, seq_len) %>%
    expand.grid() %>%
    as.matrix()

  # generate method list
  if (nrow(combIdx) == 0) {
    methodList = list(method)
  }
  else {
    methodList = apply(combIdx, 1, function(idx) {
      methodArgs = Map('[[', allArgs, idx)
      do.call(update, c(object = method, methodArgs, envir = envir))
    })
  }

  as.lcMethods(methodList)
}
