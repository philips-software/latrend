#' Synthetic longitudinal dataset comprising three classes
#' @format A `data.frame` describing 250 trajectories originating from one of three classes,
#' each with a different cluster trajectory. Trajectories randomly deviate in intercept and slope from the reference cluster.
#' \describe{
#'   \item{Id}{trajectory identifier, `factor`.}
#'   \item{Time}{measurement time, `numeric` between 0 and 2.}
#'   \item{Y}{observed variable, `numeric`.}
#'   \item{Class}{the reference class, `factor`.}
#' }
#' @source This dataset was generated using [generateLongData].
#' @seealso [generateLongData]
"latrendData"

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
#' @examples
#' longdata <- generateLongData(sizes = c(40, 70), id = "Id",
#'                             cluster = ~poly(Time, 2, raw = TRUE),
#'                             clusterCoefs = cbind(c(1, 2, 5), c(-3, 4, .2)))
#' plotTrajectories(longdata, response = "Value", id = "Id", time = "Time")
generateLongData = function(sizes = c(40, 60),
                            fixed = Value ~ 1 + Time,
                            cluster = ~ 1 + Time,
                            random = ~ 1,
                            id = getOption('latrend.id'),
                            data = data.frame(Time = seq(0, 1, by = .1)),
                            fixedCoefs = c(0, 0),
                            clusterCoefs = cbind(c(-2, 1), c(2, -1)),
                            randomScales = cbind(.1, .1),
                            rrandom = rnorm,
                            noiseScales = c(.1, .1),
                            rnoise = rnorm,
                            clusterNames = LETTERS[seq_along(sizes)],
                            shuffle = FALSE) {
  nClus = length(sizes)
  assert_that(nClus >= 1, all(sizes > 0))
  assert_that(is.character(id) && nchar(id) > 0)
  assert_that(is.character(clusterNames), length(clusterNames) == nClus)
  assert_that(is.logical(shuffle))
  assert_that(is.data.frame(data), all(vapply(data, is.numeric, FUN.VALUE =
                                                FALSE)))

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
    assert_that(is.null(fixedCoefs) ||
                  is.numeric(fixedCoefs) && is.null(dim(fixedCoefs)))
    Xfi = model.matrix(update(fixed, NULL ~ . - 1), data) #design matrix without intercept
    assert_that(ncol(Xf) == length(fixedCoefs), msg = 'Missing or too many coefficients specified for fixed effects.')
    fixedValues = Xf %*% fixedCoefs
    alldata = data.table(
      Id = rowIds,
      Cluster = as.integer(clusters)[rowIds],
      Mu.fixed = fixedValues[rep(seq_len(nObs), nIds)],
      Xfi[rep(seq_len(nObs), nIds), , drop = FALSE],
      data[, setdiff(names(data), colnames(Xfi)), drop =
             FALSE]
    )
  } else {
    alldata = data.table(
      Id = rowIds,
      Cluster = as.integer(clusters)[rowIds],
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
  alldata[, Mu.cluster := rowSums(Xc * t(clusterCoefs)[Cluster, ])]

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

    alldata[, Mu := Mu.fixed + Mu.cluster + Mu.random]
  } else {
    alldata[, Mu := Mu.fixed + Mu.cluster]
  }


  ## Noise
  assert_that(is.numeric(noiseScales))
  if (nClus > 1 && length(noiseScales) == 1) {
    noiseScales = rep(noiseScales, nClus)
  }
  assert_that(length(noiseScales) == nClus)
  alldata[, Value := Mu + rnoise(.N, 0, noiseScales[Cluster])]

  ## Finalize
  if (shuffle) {
    newOrder = sample.int(nIds)
    alldata = alldata[order(newOrder[Id])]
  }
  setnames(alldata, 'Value', response)
  setnames(alldata, 'Id', id)
  setcolorder(alldata, c(id, 'Cluster', setdiff(colnames(Xf), '(Intercept)'), response))
  alldata[, Cluster := factor(clusterNames[Cluster], levels = clusterNames)]
  return(alldata[])
}
