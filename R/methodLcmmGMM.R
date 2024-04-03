#' @include method.R
setClass('lcMethodLcmmGMM', contains = 'lcMethod')

setValidity('lcMethodLcmmGMM', function(object) {
  if (isArgDefined(object, 'formula')) {
    f = formula(object)
    assert_that(hasSingleResponse(object$formula))

    reTerms = getREterms(f)
    assert_that(
      length(reTerms) == 1,
      msg = 'formula should contain one random-effects component'
    )
    assert_that(
      getREGroupName(reTerms[[1]]) %in% c(object$id, 'ID'),
      msg = 'Group variable in random-effects component should match the id argument, or equal "ID"'
    )
  }

  if (isArgDefined(object, 'formula.mb')) {
    assert_that(!hasResponse(formula(object, 'mb')))
  }
})

#' @export
#' @title Specify GMM method using lcmm
#' @description Growth mixture modeling through latent-class linear mixed modeling.
#' @param fixed The fixed effects formula.
#' @param mixture The mixture-specific effects formula. See [lcmm::hlme] for details.
#' @param random The random effects formula. See [lcmm::hlme] for details.
#' @param classmb The cluster membership formula for the multinomial logistic model. See [lcmm::hlme] for details.
#' @param time The name of the time variable.
#' @param id The name of the trajectory identifier variable. This replaces the `subject` argument of [lcmm::hlme].
#' @param init Alternative for the `B` argument of [lcmm::hlme], for initializing the hlme fitting procedure.
#' This is only applicable for `nClusters > 1`.
#' Options:
#' * `"lme.random"` (default): random initialization through a standard linear mixed model. Assigns a fitted standard linear mixed model enclosed in a call to random() to the `B` argument.
#' * `"lme"`, fits a standard linear mixed model and passes this to the `B` argument.
#' * `"gridsearch"`, a gridsearch is used with initialization from `"lme.random"`, following the approach used by [lcmm::gridsearch]. To use this initalization, specify arguments `gridsearch.maxiter` (max number of iterations during search), `gridsearch.rep` (number of fits during search), and `gridsearch.parallel` (whether to enable [parallel computation][latrend-parallel]).
#' * `NULL` or `"default"`, the default [lcmm::hlme] input for `B` is used.
#'
#' The argument is ignored if the `B` argument is specified, or `nClusters = 1`.
#'
#' @param nClusters The number of clusters to fit. This replaces the `ng` argument of [lcmm::hlme].
#' @param ... Arguments passed to [lcmm::hlme].
#' The following arguments are ignored: data, fixed, random, mixture, subject, classmb, returndata, ng, verbose, subset.
#' @examples
#' data(latrendData)
#'
#' if (rlang::is_installed("lcmm")) {
#'   method <- lcMethodLcmmGMM(
#'     fixed = Y ~ Time,
#'     mixture = ~ Time,
#'     random = ~ 1,
#'     id = "Id",
#'     time = "Time",
#'     nClusters = 2
#'   )
#'   gmm <- latrend(method, data = latrendData)
#'   summary(gmm)
#'
#'   # define method with gridsearch
#'   method <- lcMethodLcmmGMM(
#'     fixed = Y ~ Time,
#'     mixture = ~ Time,
#'     random = ~ 1,
#'     id = "Id",
#'     time = "Time",
#'     nClusters = 3,
#'     init = "gridsearch",
#'     gridsearch.maxiter = 10,
#'     gridsearch.rep = 50,
#'     gridsearch.parallel = TRUE
#'   )
#' }
#' @family lcMethod implementations
#' @references
#' \insertRef{proustlima2017estimation}{latrend}
#'
#' \insertRef{proustlima2019lcmm}{latrend}
lcMethodLcmmGMM = function(
  fixed,
  mixture = ~ 1,
  random = ~ 1,
  classmb = ~ 1,
  time = getOption('latrend.time'),
  id = getOption('latrend.id'),
  init = 'lme',
  nClusters = 2,
  ...
) {
  mc = match.call.all()
  mc$Class = 'lcMethodLcmmGMM'
  do.call(new, as.list(mc))
}

#' @rdname interface-lcmm
setMethod('getArgumentDefaults', 'lcMethodLcmmGMM', function(object) {
  c(
    formals(lcMethodLcmmGMM),
    formals(lcmm::hlme),
    callNextMethod()
  )
})

#' @rdname interface-lcmm
setMethod('getArgumentExclusions', 'lcMethodLcmmGMM', function(object) {
  union(
    callNextMethod(),
    c('data', 'subject', 'returndata', 'ng', 'verbose', 'subset')
  )
})

#' @rdname interface-lcmm
setMethod('getCitation', 'lcMethodLcmmGMM', function(object, ...) {
  citation('lcmm')
})

#' @rdname interface-lcmm
setMethod('getName', 'lcMethodLcmmGMM', function(object) 'growth mixture model')

#' @rdname interface-lcmm
setMethod('getShortName', 'lcMethodLcmmGMM', function(object) 'gmm')

#' @rdname interface-lcmm
setMethod('validate', 'lcMethodLcmmGMM', function(method, data, envir = NULL, ...) {
  if (hasCovariates(method$fixed) && not(timeVariable(method) %in% getCovariates(method$fixed))) {
    warning(
      sprintf(
        'The specified time variable "%s" does not occur in the %s argument for "fixed" = %s.',
        timeVariable(method),
        class(method)[1],
        deparse(method$fixed)
      ),
      immediate. = TRUE
    )
  }

  TRUE
})

gmm_prepare = function(method, data, envir, verbose, ...) {
  prepEnv = new.env()
  prepEnv$verbose = as.logical(verbose)

  # Check & process data
  id = idVariable(method)
  trainData = as.data.table(data) %>%
    .[, c(id) := factor(get(..id)) %>% as.integer()]

  # Create argument list
  args = as.list(method, args = lcmm::hlme)
  args$data = as.data.frame(trainData)
  args$subject = idVariable(method)
  args$classmb = prepEnv$classmb = dropIntercept(method$classmb) # drop intercept from formula.mb
  args$ng = method$nClusters
  args$verbose = envir$verbose
  args$returndata = TRUE

  # process mixture formula
  if (method$nClusters > 1) {
    if (length(getCovariates(method$mixture)) == 0 && !hasIntercept(method$mixture)) {
      if (method$nClusters > 1) {
        warnings.Verbose(
          verbose,
          'no cluster-specific terms specified in formula. Defaulting to intercept.'
        )
      }
      args$mixture = as.formula('~1', env = environment(method$mixture))
    } else {
      args$mixture = dropResponse(method$mixture)
    }
  } else {
    args$mixture = NULL
  }

  # classmb is not allowed to be specified for ng=1
  if (method$nClusters == 1 || !hasCovariates(args$classmb)) {
    args$classmb = NULL
  }

  if (hasName(method, 'init') && method$nClusters > 1) {
    init = match.arg(method$init, c('default', 'lme', 'lme.random', 'gridsearch'))
    if (init == 'default') {
      init = 'lme'
    }

    switch(init,
      lme = {
        cat(verbose, 'Fitting standard linear mixed model for initializing the mixture estimation...')
        args1 = args
        args1$ng = 1
        args1$mixture = NULL
        args1$classmb = NULL

        args$B = do.call(lcmm::hlme, args1)
      },
      lme.random = {
        cat(verbose, 'Fitting standard linear mixed model for random initialization for the mixture estimation...')
        args1 = args
        args1$ng = 1
        args1$mixture = NULL
        args1$classmb = NULL
        prepEnv$lme = do.call(lcmm::hlme, args1)
        args$B = quote(random(lme))
      },
      gridsearch = {
        cat(verbose, 'Fitting standard linear mixed model for gridsearch initialization...')
        args1 = args
        args1$ng = 1
        args1$mixture = NULL
        args1$classmb = NULL
        prepEnv$lme = do.call(lcmm::hlme, args1)
      }
    )
  }

  prepEnv$args = args
  prepEnv
}

#' @rdname interface-lcmm
setMethod('preFit', 'lcMethodLcmmGMM', gmm_prepare)

##
gmm_fit = function(method, data, envir, verbose, ...) {
  args = envir$args

  if (hasName(envir, 'lme')) {
    # work-around for eval() of hlme() only considering global and function scope
    .latrend.lme <- envir$lme
    # args$B = quote(random(get('.latrend.lme', envir = parent.frame(3))))
    args$B = quote(random(dynGet('.latrend.lme', inherits = TRUE)))
  }

  model = do.call(lcmm::hlme, args)

  model$fixed = args$fixed
  model$mixture = args$mixture
  model$random = args$random
  model$mb = envir$classmb
  model
}


gmm_gridsearch = function(method, data, envir, verbose, ...) {
  assert_that(
    is.count(method$gridsearch.maxiter),
    is.count(method$gridsearch.rep),
    is.flag(method$gridsearch.parallel),
    hasName(envir, 'lme'),
    inherits(envir$lme, 'hlme')
  )

  args = envir$args
  gridArgs = args
  gridArgs$maxiter = method$gridsearch.maxiter
  rep = method$gridsearch.rep
  .latrend.lme = envir$lme
  `%infix%` = ifelse(method$gridsearch.parallel, `%dopar%`, `%do%`)

  # Conduct gridsearch
  timing = .enterTimed(verbose, sprintf('Gridsearch with %d repetitions...', rep))
  gridModels = foreach(k = seq_len(rep)) %infix% {
    cat(
      verbose,
      sprintf('Gridsearch fit %d/%d (%g%%)', k, rep, round(k / rep * 100))
    )
    e = environment()
    assign('minit', .latrend.lme, envir = e)
    gridArgs$B = substitute(random(minit), env = e)
    gridModel = do.call(lcmm::hlme, gridArgs)
    gridModel
  }
  .exitTimed(timing)

  # determine the best candidate solution
  gridLogLiks = vapply(gridModels, function(x) x$loglik, FUN.VALUE = 0)
  assert_that(
    any(is.finite(gridLogLiks)),
    msg = 'Failed to obtain a valid fit during gridsearch. Try more reps or higher maxiter?'
  )
  iBest = which.max(gridLogLiks)
  args$B = gridModels[[iBest]]$best

  # fit the final model
  timing = .enterTimed(verbose, 'Final model optimization...')
  model = do.call(lcmm::hlme, args)
  .exitTimed(timing, msg = 'Done with final model optimization (%s)')

  #
  model$fixed = args$fixed
  model$mixture = args$mixture
  model$random = args$random
  model$mb = envir$classmb
  model
}


#' @rdname interface-lcmm
setMethod('fit', 'lcMethodLcmmGMM', function(method, data, envir, verbose, ...) {
  if (method$init == 'gridsearch' && method$nClusters > 1) {
    model = gmm_gridsearch(method, data, envir, verbose, ...)
  } else {
    model = gmm_fit(method, data, envir, verbose, ...)
  }

  new(
    'lcModelLcmmGMM',
    method = method,
    data = data,
    model = model,
    clusterNames = make.clusterNames(method$nClusters)
  )
})


#' @rdname interface-lcmm
setMethod('responseVariable', 'lcMethodLcmmGMM', function(object, ...) {
  getResponse(object$fixed)
})
