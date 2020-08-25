#' @include method.R
setClass('lcMethodLcmmGMM', contains = 'lcMethod')

setValidity('lcMethodLcmmGMM', function(object) {
  if (isArgDefined(object, 'formula')) {
    f = formula(object)
    assert_that(hasSingleResponse(object$formula))

    reTerms = getREterms(f)
    assert_that(length(reTerms) == 1, msg = 'formula should contain one random-effects component')
    assert_that(getREGroupName(reTerms[[1]]) %in% c(object$id, 'ID'), msg =
                  'Group variable in random-effects component should match the id argument, or equal "ID"')

  }

  if (isArgDefined(object, 'formula.mb')) {
    assert_that(!hasResponse(formula(object, 'mb')))
  }
})

#' @export
#' @title Specify GMM method using lcmm
#' @description Growth mixture modeling through latent-class linear mixed modeling.
#' @param formula A `formula` of the form `Response ~ Var1 + CLUSTER * Var2 + . + (Random1 + Random2 + . | Id)`.
#' Variables specified in the model are included as fixed effects.
#' If an interaction is specified with the `CLUSTER` term, then these covariates are included as fixed and mixture effects.
#' The formula must contain a single random-effects component of the form  `(. | Id)`, where `Id` matches the name specified in the `id` argument, or `ID` (which will be substituted by the `id` argument).
#' The random effects are cluster-specific.
#' @param formula.mb A `formula` specifying the class membership model. By default, an intercept-only model is used. This is a replacement of the internal `classmb` argument in [lcmm::lcmm].
#' @param time The name of the time variable.
#' @param id The name of the trajectory identifier variable. This replaces the `subject` argument of [lcmm::lcmm].
#' @param nClusters The number of clusters to fit. This replaces the `ng` argument of [lcmm::lcmm].
#' @param ... Arguments passed to [lcmm::lcmm].
#' The following arguments are ignored: data, fixed, random, mixture, subject, classmb, returndata, ng, verbose, subset.
#' @details The `formula` argument is used to generate the `fixed`, `random`, and `mixture` arguments for [lcmm::lcmm].
#' @examples
#' library(lcmm)
#' data(latrendData)
#' method <- lcMethodLcmmGMM(Y ~ Time * CLUSTER + (1 | Id),
#'                      time = "Time", id = "Id", nClusters = 3)
#' gmm <- latrend(method, data = latrendData)
#' summary(gmm)
#'
#' method <- lcMethodLcmmGMM(Y ~ Time * CLUSTER + (Time | Id),
#'                      time = "Time", id = "Id", nClusters = 3)
#' @family lcMethod implementations
lcMethodLcmmGMM = function(formula,
                           formula.mb =  ~ 1,
                           time = getOption('latrend.time'),
                           id = getOption('latrend.id'),
                           nClusters = 2,
                           ...) {
  lcMethod.call(
    'lcMethodLcmmGMM',
    call = match.call.defaults(),
    defaults = lcmm::lcmm,
    excludeArgs = c(
      'data',
      'fixed',
      'random',
      'mixture',
      'subject',
      'classmb',
      'returndata',
      'ng',
      'verbose',
      'subset'
    )
  )
}

#' @rdname interface-lcmm
setMethod('getName', signature('lcMethodLcmmGMM'), function(object) 'growth mixture model')

#' @rdname interface-lcmm
setMethod('getShortName', signature('lcMethodLcmmGMM'), function(object) 'gmm')

gmm_prepare = function(method, data, envir, verbose, ...) {
  e = new.env()

  f = formula(method)
  valueColumn = getResponse(f)
  e$verbose = as.logical(verbose)

  # Check & process data
  id = idVariable(method)
  e$data = as.data.table(data) %>%
    .[, c(id) := factor(get(id)) %>% as.integer]

  # Parameter processing
  e$fixed = dropRE(f) %>% dropCLUSTER
  e$mixture = dropResponse(f) %>% dropRE %>% keepCLUSTER
  if (length(getCovariates(e$mixture)) == 0 &&
      !hasIntercept(e$mixture)) {
    if (method$nClusters > 1) {
      warnings.Verbose(
        verbose,
        'no cluster-specific terms specified in formula. Defaulting to intercept.'
      )
    }
    e$mixture = as.formula('~1', env = environment(e$mixture))
  }

  reTerms = getREterms(f)
  if (length(reTerms) > 0) {
    e$random = reTerms[[1]] %>% REtermAsFormula
  }

  cat(verbose, sprintf('\tfixed: %s', deparse(e$fixed)), level = verboseLevels$finest)
  cat(verbose, sprintf('\tmixture: %s', deparse(e$mixture)), level = verboseLevels$finest)
  cat(verbose, sprintf('\trandom: %s', deparse(e$random)), level = verboseLevels$finest)

  # drop intercept from formula.mb
  e$formula.mb = formula(method, what = 'mb') %>% dropIntercept

  return(e)
}
#' @rdname interface-lcmm
setMethod('preFit', signature('lcMethodLcmmGMM'), gmm_prepare)

##
gmm_fit = function(method, data, envir, verbose, ...) {
  args = as.list(method, args = lcmm::lcmm)
  args$data = as.data.frame(envir$data)
  args$fixed = envir$fixed
  if (method$nClusters > 1) {
    args$mixture = envir$mixture
  } else {
    args$mixture = NULL
  }
  args$random = envir$random
  args$subject = idVariable(method)
  args$classmb = envir$formula.mb
  args$ng = method$nClusters
  args$verbose = envir$verbose
  args$returndata = TRUE

  if (method$nClusters == 1 || !hasCovariates(args$classmb)) {
    # classmb is not allowed to be specified for ng=1
    args$classmb = NULL
  }

  model = do.call(lcmm::lcmm, args)

  model$fixed = envir$fixed
  model$mixture = envir$mixture
  model$random = envir$random
  model$mb = envir$formula.mb

  return(model)
}

#' @rdname interface-lcmm
setMethod('fit', signature('lcMethodLcmmGMM'), function(method, data, envir, verbose, ...) {
  model = gmm_fit(method, data, envir, verbose, ...)

  new(
    'lcModelLcmmGMM',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})
