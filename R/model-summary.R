#' @include model.R

setClass(
  'lcSummary',
  representation(
    method = 'lcMethod',
    name = 'character',
    nClusters = 'integer',
    nObs = 'numeric',
    id = 'character',
    coefficients = 'ANY',
    residuals = 'numeric',
    clusterNames = 'character',
    trajectoryAssignments = 'factor',
    clusterSizes = 'numeric',
    clusterProportions = 'numeric',
    metrics = 'numeric'
  )
)


#' @export
#' @title Summarize a lcModel
#' @description Extracts all relevant information from the underlying model into a list
#' @param object The `lcModel` object.
#' @param ... Additional arguments.
summary.lcModel = function(object, ...) {
  res = residuals(object)
  if (is.null(res)) {
    res = as.numeric(NA)
  }

  new(
    'lcSummary',
    method = getLcMethod(object),
    name = getName(object),
    nClusters = nClusters(object),
    nObs = ifelse(is.null(nobs(object)), 0L, nobs(object)),
    id = idVariable(object),
    coefficients = coef(object),
    residuals = res,
    clusterNames = clusterNames(object),
    trajectoryAssignments = trajectoryAssignments(object),
    clusterSizes = clusterSizes(object),
    clusterProportions = clusterProportions(object)
  )
}


# . show ####
setMethod('show', 'lcSummary',
  function(object) {
    cat('Longitudinal cluster model using ', object@name, '\n', sep = '')
    print(object@method)
    cat('\n')
    sprintf('Cluster sizes (K=%d):\n', object@nClusters) %>% cat
    sprintf('%g (%g%%)',
      object@clusterSizes,
      round(object@clusterProportions * 100, 1)) %>%
      setNames(object@clusterNames) %>%
      noquote %>%
      print
    cat('\n')
    sprintf(
      'Number of obs: %d, strata (%s): %d\n',
      object@nObs,
      object@id,
      length(object@trajectoryAssignments)
    ) %>% cat
    cat('\n')
    cat('Scaled residuals:\n')
    object@residuals %>% scale %>% as.vector %>% summary %>% print
    cat('\n')
})
