setClass('clMethodGMM', contains='clMethod')

#' @export
#' @importFrom lcmm hlme
#' @title Specify GMM method
#' @description Growth mixture modeling through latent-class linear mixed modeling.
#' @inheritParams clMethodKML
#' @param formula Formula of form Response ~ Var1 + CLUSTER * Var2 + . + (Random1 + Random2 + . | Id).
#' Variables specified in the model are included as fixed effects.
#' If an interaction is specified with the CLUSTER term then these covariates are included as fixed and mixture effects.
#' The formula must contain a single random-effects component of the form  (. | Id), where Id matches the name specified in id, or in case of 'ID' is replaced by the id argument.
#' Random effects are cluster-specific.
#' @param formula.mb Formula for the multinomial class membership model.
#' @param diagCov Whether to use a diagonal variance-covariance matrix
#' @param classCov Whether to use a class-specific variance-covariance matrix
#' @inheritParams lcmm::hlme
#' @examples
#' method = clMethodGMM(Value ~ CLUSTER + (1 | Subject),
#'                      time='Assessment',
#'                      id='Subject', nClusters=3)
#' gmm = cluslong(method, data=testLongData)
#' summary(gmm)
clMethodGMM = function(formula=Value ~ 1 + CLUSTER + (1 | ID),
                       formula.mb=~1,
                       time=getOption('cluslong.time'),
                       id=getOption('cluslong.id'),
                       nClusters=2,
                       maxIter=500,
                       idiag=FALSE,
                       nwg=FALSE,
                       cor=NULL,
                       convB=1e-4,
                       convL=1e-4,
                       convG=1e-4) {
  object = new('clMethodGMM', call=match.call.defaults())

  if(getOption('cluslong.checkArgs')) {
    checkArgs(object, envir=parent.frame())
  }

  return(object)
}

setMethod('checkArgs', signature('clMethodGMM'), function(object, envir) {
  environment(object) = envir
  assert_that(all(formalArgs(clMethodGMM) %in% names(getCall(object))), msg='clMethod object is missing required arguments')

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

  if(isArgDefined(object, 'nClusters')) {
    assert_that(is.count(object$nClusters))
  }

  if(isArgDefined(object, 'maxIter')) {
    assert_that(is.count(object$maxIter))
  }
})


setMethod('getName', signature('clMethodGMM'), function(object) 'growth mixture model')

setMethod('getName0', signature('clMethodGMM'), function(object) 'gmm')

gmm_prepare = function(method, data, control) {
  e = new.env()

  f = formula(method)
  valueColumn = getResponse(f)
  e$verbose = getLogger()$getLevel() < loglevels['INFO']

  # Check & process data
  e$data = as.data.table(data) %>%
    .[, c(method$id) := factor(get(method$id)) %>% as.integer]

  # Parameter processing
  vars = terms(f) %>% labels
  e$fixed = dropRE(f) %>% dropCLUSTER
  e$mixture = dropResponse(f) %>% dropRE %>% keepCLUSTER
  if (length(getCovariates(e$mixture)) == 0 && !hasIntercept(e$mixture)) {
    if (method$nClusters > 1) {
      warning('no cluster-specific terms specified in formula. Defaulting to intercept.', immediate.=TRUE)
    }
    e$mixture = as.formula('~1', env=environment(e$mixture))
  }

  reTerms = getREterms(f)
  if (length(reTerms) > 0) {
    e$random = reTerms[[1]] %>% REtermAsFormula
  }

  logfinest(sprintf('\tfixed: %s', deparse(e$fixed)))
  logfinest(sprintf('\tmixture: %s', deparse(e$mixture)))
  logfinest(sprintf('\trandom: %s', deparse(e$random)))

  # drop intercept from formula.mb
  e$formula.mb = formula(method, what='mb') %>% dropIntercept

  # Model specification

  return(e)
}
setMethod('prepare', signature('clMethodGMM'), gmm_prepare)

##
gmm_fit = function(method, data, control, prepEnv) {
  e = new.env(parent=prepEnv)

  valueColumn = formula(method) %>% getResponse

  args = as.list(method)
  args$data = prepEnv$data
  args$fixed = prepEnv$fixed
  args$maxiter = method$maxIter
  if (method$nClusters > 1) {
    args$mixture = prepEnv$mixture
  }
  args$random = prepEnv$random
  args$subject = method$id
  args$classmb = prepEnv$formula.mb
  args$ng = method$nClusters
  args$verbose = prepEnv$verbose
  args[setdiff(names(args), formalArgs(hlme))] = NULL #remove undefined arguments
  args$returndata = TRUE

  startTime = Sys.time()
  model = do.call(hlme, args)

  e$runTime = as.numeric(Sys.time() - startTime)

  model$fixed = prepEnv$fixed
  model$mixture = prepEnv$mixture
  model$random = prepEnv$random
  model$mb = prepEnv$formula.mb
  e$model = model

  return(e)
}
setMethod('fit', signature('clMethodGMM'), gmm_fit)


##
gmm_finalize = function(method, data, control, fitEnv) {
  model = new('clModelGMM',
              method=method,
              model=fitEnv$model,
              clusterNames=genClusNames(method$nClusters))
  return(model)
}
setMethod('finalize', signature('clMethodGMM'), gmm_finalize)