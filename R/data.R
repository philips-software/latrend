#' @name latrend-data
#' @title Longitudinal dataset representation
#' @description The [latrend estimation functions][latrend-estimation] expect univariate longitudinal data that can be represented in a `data.frame` with one row per trajectory observation:
#' * Trajectory identifier: `numeric`, `character`, or `factor`
#' * Observation time: `numeric`
#' * Observation value: `numeric`
#'
#' In principle, any type of longitudinal data structure is supported, given that it can be transformed to the required `data.frame` format using the generic [trajectories] function.
#' Support can be added by implementing the [trajectories] function for the respective signature.
#' This means that users can implement their own data adapters as needed.
#'
#' @section Included longitudinal datasets:
#' The following datasets are included with the package:
#' * [latrendData]
#' * [PAP.adh]
#' * [PAP.adh1y]
NULL


#' @title Artificial longitudinal dataset comprising three classes
#' @description
#' An artificial longitudinal dataset comprising 200 trajectories belonging to one of 3 classes.
#' Each trajectory deviates in intercept and slope from its respective class trajectory.
#' @format A `data.frame` comprising longitudinal observations from 200 trajectories.
#' Each row represents the observed value of a trajectory at a specific moment in time.
#' \describe{
#'   \item{Id}{`integer`: The trajectory identifier.}
#'   \item{Time}{`numeric`: The measurement time, between 0 and 2.}
#'   \item{Y}{`numeric`: The observed value at the respective time `Time` for trajectory `Id`.}
#'   \item{Class}{`factor`: The reference class.}
#' }
#'
#' ```{r}
#' data(latrendData)
#' head(latrendData)
#' ```
#'
#' @source This dataset was generated using [generateLongData].
#' @seealso [latrend-data] [generateLongData]
#' @examples
#' data(latrendData)
#'
#' if (require("ggplot2")) {
#'   plotTrajectories(latrendData, id = "Id", time = "Time", response = "Y")
#'
#'   # plot according to the reference class
#'   plotTrajectories(latrendData, id = "Id", time = "Time", response = "Y", cluster = "Class")
#' }
"latrendData"


#' @name PAP.adh
#' @title Biweekly Mean Therapy Adherence of OSA Patients over 1 Year
#' @description
#' A simulated longitudinal dataset comprising 500 patients with obstructive sleep apnea (OSA) during their
#' first year on CPAP therapy.
#' The dataset contains the patient usage hours, averaged over 2-week periods.
#'
#' The daily usage data underlying the downsampled dataset was simulated based on 7 different adherence patterns.
#' The defined adherence patterns were inspired by the adherence patterns identified by Aloia et al. (2008), with slight adjustments
#' @format A `data.frame` comprising longitudinal data of 500 patients, each having 26 observations over a period of 1 year.
#' Each row represents a patient observation interval (two weeks), with columns:
#' \describe{
#'   \item{Patient}{`factor`: The patient identifier, where each level represents a simulated patient.}
#'   \item{Biweek}{`integer`: Two-week interval index. Starts from 1.}
#'   \item{MaxDay}{`integer`: The last day used for the aggregation of the respective interval, `integer`}
#'   \item{UsageHours}{`numeric`: The mean hours of usage in the respective week.
#'   Greater than or equal to zero, and typically around 4-6 hours.}
#'   \item{Group}{`factor`: The reference group (i.e., adherence pattern) from which this patient was generated.}
#' }
#'
#' @note This dataset is only intended for demonstration purposes.
#' While the data format will remain the same, the data content is subject to change in future versions.
#' @source This dataset was generated based on the cluster-specific descriptive statistics table provided in Aloia et al. (2008),
#' with some adjustments made in order to improve cluster separation for demonstration purposes.
#'
#' \insertRef{aloia2008time}{latrend}
#' @seealso [latrend-data]
#' @examples
#' data(PAP.adh)
#'
#' if (require("ggplot2")) {
#'   plotTrajectories(PAP.adh, id = "Patient", time = "Biweek", response = "UsageHours")
#'
#'   # plot according to cluster ground truth
#'   plotTrajectories(
#'     PAP.adh,
#'     id = "Patient",
#'     time = "Biweek",
#'     response = "UsageHours",
#'     cluster = "Group"
#'   )
#' }
"PAP.adh"

#' @rdname PAP.adh
#' @description The `PAP.adh1y` dataset is a subset of `PAP.adh`, comprising only patients who used therapy for at least 1 year.
#' The subset does not contain the Non-users and Early drop-out groups.
"PAP.adh1y"

#' @export
#' @title Generate longitudinal test data
#' @param sizes Number of strata per cluster.
#' @param fixed Fixed effects formula.
#' @param random Random effects formula.
#' @param cluster Cluster effects formula.
#' @param id Name of the strata.
#' @param data Data with covariates to use for generation. Stratified data may be specified by adding a grouping column.
#' @param clusterNames A `character` vector denoting the names of the generated clusters.
#' @param fixedCoefs Coefficients matrix for the fixed effects.
#' @param clusterCoefs Coefficients matrix for the cluster effects.
#' @param randomScales Standard deviations matrix for the size of the variance components (random effects).
#' @param rrandom Random sampler for generating the variance components at location 0.
#' @param noiseScales Scale of the random noise passed to rnoise. Either scalar or defined per cluster.
#' @param rnoise Random sampler for generating noise at location 0 with the respective scale.
#' @param shuffle Whether to randomly reorder the strata in which they appear in the data.frame.
#' @param seed Optional seed to set for the PRNG. The set PRNG state persists after the function completes.
#' @seealso [latrend-data]
#' @examples
#' longdata <- generateLongData(
#'   sizes = c(40, 70), id = "Id",
#'   cluster = ~poly(Time, 2, raw = TRUE),
#'   clusterCoefs = cbind(c(1, 2, 5), c(-3, 4, .2))
#' )
#'
#' if (require("ggplot2")) {
#'   plotTrajectories(longdata, response = "Value", id = "Id", time = "Time")
#' }
generateLongData = function(
  sizes = c(40, 60),
  fixed = Value ~ 1,
  cluster = ~ 1 + Time,
  random = ~ 1,
  id = getOption('latrend.id'),
  data = data.frame(Time = seq(0, 1, by = .1)),
  fixedCoefs = 0,
  clusterCoefs = cbind(c(-2, 1), c(2, -1)),
  randomScales = cbind(.1, .1),
  rrandom = rnorm,
  noiseScales = c(.1, .1),
  rnoise = rnorm,
  clusterNames = LETTERS[seq_along(sizes)],
  shuffle = FALSE,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  nClus = length(sizes)
  assert_that(nClus >= 1, all(sizes > 0))
  assert_that(is.character(id) && nchar(id) > 0)
  assert_that(is.character(clusterNames), length(clusterNames) == nClus)
  assert_that(is.logical(shuffle))
  assert_that(is.data.frame(data), all(vapply(data, is.numeric, FUN.VALUE = FALSE)))

  ## Fixed effects
  assert_that(is.formula(fixed), hasSingleResponse(fixed))
  nIds = sum(sizes)
  ids = seq_len(nIds)
  nObs = nrow(data)
  clusters = rep(clusterNames, sizes) %>% factor(levels = clusterNames)
  nTotalObs = nIds * nrow(data)
  response = getResponse(fixed)
  rowIds = rep(ids, each = nObs)

  Xf = model.matrix(dropResponse(fixed), data)
  if (ncol(Xf) > 0) {
    assert_that(is.null(fixedCoefs) || is.numeric(fixedCoefs) && is.null(dim(fixedCoefs)))
    Xfi = model.matrix(update(fixed, NULL ~ . - 1), data) #design matrix without intercept
    assert_that(
      ncol(Xf) == length(fixedCoefs),
      msg = 'Missing or too many coefficients specified for fixed effects.'
    )
    fixedValues = Xf %*% fixedCoefs
    alldata = data.table(
      Id = rowIds,
      Class = as.integer(clusters)[rowIds],
      Mu.fixed = fixedValues[rep(seq_len(nObs), nIds)],
      Xfi[rep(seq_len(nObs), nIds), , drop = FALSE],
      data[, setdiff(names(data), colnames(Xfi)), drop = FALSE]
    )
  } else {
    alldata = data.table(
      Id = rowIds,
      Class = as.integer(clusters)[rowIds],
      Mu.fixed = 0,
      data
    )
  }



  ## Cluster effects
  assert_that(
    is.formula(cluster),
    !hasResponse(cluster),
    is.numeric(clusterCoefs),
    is.matrix(clusterCoefs)
  )
  assert_that(ncol(clusterCoefs) == nClus)
  Xc = model.matrix(cluster, alldata)
  assert_that(ncol(Xc) == nrow(clusterCoefs), msg = 'Missing or too many coefficients specified for cluster effects.')
  alldata[, Mu.class := rowSums(Xc * t(clusterCoefs)[Class, ])]

  ## Random effects
  assert_that(is.formula(random), !hasResponse(random))
  Xr = model.matrix(random, alldata)
  if (ncol(Xr) > 0) {
    if (!is.matrix(randomScales)) {
      randomScales = matrix(randomScales,
                            nrow = length(randomScales),
                            ncol = nClus)
    }
    assert_that(
      is.matrix(randomScales),
      ncol(randomScales) == nClus,
      nrow(randomScales) == ncol(Xr)
    )
    # generate id-specific scales
    idScales = t(randomScales)[rep(1:nClus, sizes), ] %>%
      matrix(ncol = nrow(randomScales))
    assert_that(nrow(idScales) == nIds)
    idCoefs = rrandom(nIds * nrow(randomScales), 0, idScales) %>%
      matrix(ncol = nrow(randomScales))
    assert_that(nrow(idCoefs) == nIds)
    alldata[, Mu.random := rowSums(Xr * idCoefs[Id, ])]

    alldata[, Mu := Mu.fixed + Mu.class + Mu.random]
  } else {
    alldata[, Mu := Mu.fixed + Mu.class]
  }


  ## Noise
  assert_that(is.numeric(noiseScales))
  if (nClus > 1 && length(noiseScales) == 1) {
    noiseScales = rep(noiseScales, nClus)
  }
  assert_that(length(noiseScales) == nClus)
  alldata[, Value := Mu + rnoise(.N, 0, noiseScales[Class])]

  ## Finalize
  if (shuffle) {
    newOrder = sample.int(nIds)
    alldata = alldata[order(newOrder[Id])]
  }
  setnames(alldata, 'Value', response)
  setnames(alldata, 'Id', id)
  setcolorder(alldata, c(id, 'Class', setdiff(colnames(Xf), '(Intercept)'), response))
  alldata[, Class := factor(clusterNames[Class], levels = clusterNames)]
  return(alldata[])
}
