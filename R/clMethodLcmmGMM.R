#' @include clMethod.R
setClass('clMethodLcmmGMM', contains='clMethod')

setValidity('clMethodLcmmGMM', function(object) {
  if(isArgDefined(object, 'formula')) {
    f = formula(object)
    assert_that(hasSingleResponse(object$formula))

    reTerms = getREterms(f)
    assert_that(length(reTerms) == 1, msg='formula should contain one random-effects component')
    assert_that(getREGroupName(reTerms[[1]]) %in% c(object$id, 'ID'), msg='Group variable in random-effects component should match the id argument, or equal "ID"')

  }

  if(isArgDefined(object, 'formula.mb')) {
    assert_that(!hasResponse(formula(object, 'mb')))
  }
})

#' @export
#' @importFrom lcmm lcmm
#' @title Specify GMM method using lcmm
#' @description Growth mixture modeling through latent-class linear mixed modeling.
#' @inheritParams clMethodKML
#' @param formula Formula of form Response ~ Var1 + CLUSTER * Var2 + . + (Random1 + Random2 + . | Id).
#' Variables specified in the model are included as fixed effects.
#' If an interaction is specified with the CLUSTER term then these covariates are included as fixed and mixture effects.
#' The formula must contain a single random-effects component of the form  (. | Id), where Id matches the name specified in id, or in case of 'ID' is replaced by the id argument.
#' Random effects are cluster-specific.
#' @inheritDotParams lcmm::lcmm
#' @examples
#' method = clMethodLcmmGMM(Value ~ Time * CLUSTER + (1 | Id),
#'                      time='Time',
#'                      id='Id', nClusters=3)
#' gmm = cluslong(method, data=testLongData)
#' summary(gmm)
#' @family clMethod implementations
clMethodLcmmGMM = function(formula=Value ~ 1 + CLUSTER + (1 | ID),
                       formula.mb=~1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       ...
) {
  clMethod('clMethodLcmmGMM', call=match.call.defaults(),
           defaults=lcmm::lcmm,
           excludeArgs=c('data', 'fixed', 'random', 'mixture', 'subject', 'classmb', 'returndata', 'ng', 'verbose', 'subset'))
}


setMethod('getName', signature('clMethodLcmmGMM'), function(object) 'growth mixture model')

setMethod('getShortName', signature('clMethodLcmmGMM'), function(object) 'gmm')

gmm_prepare = function(method, data, verbose, ...) {
  e = new.env()

  f = formula(method)
  valueColumn = getResponse(f)
  e$verbose = as.logical(verbose)

  # Check & process data
  e$data = as.data.table(data) %>%
    .[, c(method$id) := factor(get(method$id)) %>% as.integer]

  # Parameter processing
  vars = terms(f) %>% labels
  e$fixed = dropRE(f) %>% dropCLUSTER
  e$mixture = dropResponse(f) %>% dropRE %>% keepCLUSTER
  if (length(getCovariates(e$mixture)) == 0 && !hasIntercept(e$mixture)) {
    if (method$nClusters > 1) {
      warning.Verbose(verbose, 'no cluster-specific terms specified in formula. Defaulting to intercept.')
    }
    e$mixture = as.formula('~1', env=environment(e$mixture))
  }

  reTerms = getREterms(f)
  if (length(reTerms) > 0) {
    e$random = reTerms[[1]] %>% REtermAsFormula
  }

  cat(verbose, sprintf('\tfixed: %s', deparse(e$fixed)), level=verboseLevels$finest)
  cat(verbose, sprintf('\tmixture: %s', deparse(e$mixture)), level=verboseLevels$finest)
  cat(verbose, sprintf('\trandom: %s', deparse(e$random)), level=verboseLevels$finest)

  # drop intercept from formula.mb
  e$formula.mb = formula(method, what='mb') %>% dropIntercept

  return(e)
}
setMethod('prepare', signature('clMethodLcmmGMM'), gmm_prepare)

##
gmm_fit = function(method, data, envir, verbose, ...) {
  valueColumn = formula(method) %>% getResponse

  args = as.list(method)
  args$data = envir$data
  args$fixed = envir$fixed
  if (method$nClusters > 1) {
    args$mixture = envir$mixture
  } else {
    args$mixture = NULL
  }
  args$random = envir$random
  args$subject = method$id
  args$classmb = envir$formula.mb
  args$ng = method$nClusters
  args$verbose = envir$verbose
  args[setdiff(names(args), formalArgs(lcmm))] = NULL #remove undefined arguments
  args$returndata = TRUE

  if(method$nClusters == 1 || !hasCovariates(args$classmb)) {
    # classmb is not allowed to be specified for ng=1
    args$classmb = NULL
  }

  model = do.call(lcmm, args)

  model$fixed = envir$fixed
  model$mixture = envir$mixture
  model$random = envir$random
  model$mb = envir$formula.mb

  return(model)
}

setMethod('fit', signature('clMethodLcmmGMM'), function(method, data, envir, verbose, ...) {
  model = gmm_fit(method, data, envir, verbose, ...)

  new('clModelLcmmGMM',
      method=method,
      data=data,
      model=model,
      clusterNames=make.clusterNames(method$nClusters))
})
