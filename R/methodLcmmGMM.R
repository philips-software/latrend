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
#' @param time The name of the time variable.
#' @param id The name of the trajectory identifier variable. This replaces the `subject` argument of [lcmm::hlme].
#' @param nClusters The number of clusters to fit. This replaces the `ng` argument of [lcmm::hlme].
#' @param ... Arguments passed to [lcmm::hlme].
#' The following arguments are ignored: data, fixed, random, mixture, subject, classmb, returndata, ng, verbose, subset.
#' @examples
#' data(latrendData)
#' method <- lcMethodLcmmGMM(fixed = Y ~ Time,
#'    mixture = ~ Time, random = ~ 1,
#'    id = "Id", time = "Time", , nClusters = 3)
#' gmm <- latrend(method, data = latrendData)
#' summary(gmm)
#'
#' method <- lcMethodLcmmGMM(fixed = Y ~ Time,
#'     mixture = ~ Time, random = ~ Time,
#'     id = "Id", time = "Time", nClusters = 3)
#' @family lcMethod implementations
lcMethodLcmmGMM = function(fixed,
                          mixture = ~ 1,
                          random = ~ 1,
                          classmb = ~ 1,
                          time = getOption('latrend.time'),
                          id = getOption('latrend.id'),
                          nClusters = 2,
                          ...) {
  lcMethod.call(
    'lcMethodLcmmGMM',
    call = match.call.all(),
    defaults = lcmm::hlme,
    excludeArgs = c(
      'data',
      'subject',
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
  e$verbose = as.logical(verbose)

  # Check & process data
  id = idVariable(method)
  e$data = as.data.table(data) %>%
    .[, c(id) := factor(get(id)) %>% as.integer()]

  # Parameter processing
  if (length(getCovariates(method$mixture)) == 0 && !hasIntercept(method$mixture)) {
    if (method$nClusters > 1) {
      warnings.Verbose(
        verbose,
        'no cluster-specific terms specified in formula. Defaulting to intercept.'
      )
    }
    e$mixture = as.formula('~1', env = environment(method$mixture))
  } else {
    e$mixture = method$mixture
  }

  # drop intercept from formula.mb
  e$classmb = dropIntercept(method$classmb)

  return(e)
}
#' @rdname interface-lcmm
setMethod('preFit', signature('lcMethodLcmmGMM'), gmm_prepare)

##
gmm_fit = function(method, data, envir, verbose, ...) {
  args = as.list(method, args = lcmm::hlme)
  args$data = as.data.frame(envir$data)
  if (method$nClusters > 1) {
    args$mixture = envir$mixture
  } else {
    args$mixture = NULL
  }
  args$subject = idVariable(method)
  args$classmb = envir$classmb
  args$ng = method$nClusters
  args$verbose = envir$verbose
  args$returndata = TRUE

  if (method$nClusters == 1 || !hasCovariates(args$classmb)) {
    # classmb is not allowed to be specified for ng=1
    args$classmb = NULL
  }

  model = do.call(lcmm::hlme, args)

  model$fixed = args$fixed
  model$mixture = args$mixture
  model$random = args$random
  model$mb = envir$classmb

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


#' @rdname interface-lcmm
setMethod('responseVariable', signature('lcMethodLcmmGMM'), function(object, ...) {
  getResponse(object$fixed)
})
