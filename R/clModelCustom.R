#' @include clModel.R
setClassUnion('functionOrNULL', members=c('function', 'NULL'))
.clModelCustom = setClass('clModelCustom',
         representation(clusterAssignments='integer',
                        clusterTrajectories='data.frame',
                        trajectories='data.frame',
                        converged='numeric',
                        postprob='matrix',
                        name='character',
                        predict='functionOrNULL',
                        predictPostprob='functionOrNULL'),
         contains='clModel')

#' @export
#' @title Specify a model based on a pre-computed result.
#' @param data The data on which the cluster result is based, a data.frame.
#' @param clusterAssignments A vector indicating cluster membership per strata. Either a numeric vector with range 1:numClus, or a factor.
#' @param clusterTrajectories The cluster trajectories as a data.frame, or a function computing the center trajectory based on the strata of the respective cluster.
#' @param trajectories The fitted trajectories.
#' @param response The response variable.
#' @param time The time variable.
#' @param id The id variable.
#' @param converged Convergence state of the model. TRUE by default.
#' @param postprob Optional posterior probability matrix.
#' @param predict Predict function for the response.
#' @param predictPostprob Predict function for the posterior probability.
clModelCustom = function(data,
                         clusterAssignments=NULL,
                         clusterTrajectories=mean,
                         trajectories=data,
                         response=getOption('cluslong.response'),
                         time=getOption('cluslong.time'),
                         id=getOption('cluslong.id'),
                         clusterNames=NULL,
                         converged=TRUE,
                         postprob=NULL,
                         method=new('clMethod'),
                         model=NULL,
                         name='custom',
                         predict=NULL, predictPostprob=NULL) {
  call = match.call()

  # Data
  assert_that(is.data.frame(data))
  assert_that(is.scalar(time))
  assert_that(is.scalar(id))
  assert_that(has_name(data, response))
  assert_that(has_name(data, id))
  assert_that(has_name(data, time))
  nIds = uniqueN(data[[id]])
  times = unique(data[[time]]) %>% sort

  # postprob
  if(!is.null(postprob)) {
    assert_that(is.matrix(postprob))
    assert_that(nrow(postprob) == nIds)
    assert_that(!anyNA(postprob))
    assert_that(min(postprob) >= 0)
    assert_that(max(postprob) <= 1)
    assert_that(all(rowSums(postprob) == 1))
  }

  # Cluster assignments
  if(is.null(clusterAssignments)) {
    assert_that(!is.null(postprob), msg='postprob must be specified when clusterAssignments is null')
    clusterAssignments = apply(postprob, 1, which.max)
    nClusters = ncol(postprob)
    if(is.null(clusterNames)) {
      clusterNames = colnames(postprob)
    }
  } else {
    assert_that(is.factor(clusterAssignments) || vapply(clusterAssignments, is.count, FUN.VALUE=FALSE))
    assert_that(!anyNA(clusterAssignments))
    assert_that(length(clusterAssignments) == nIds)
    if(is.null(clusterNames) && is.factor(clusterAssignments)) {
      clusterNames = levels(clusterAssignments)
    }
    clusterAssignments = as.integer(clusterAssignments)
    nClusters = max(clusterAssignments)
  }
  assert_that(nClusters >= 1)

  # postprob generation
  if(is.null(postprob)) {
    postprob = matrix(0, nrow=nIds, ncol=nClusters)
    idxMat = cbind(1:nIds, clusterAssignments)
    postprob[idxMat] = 1
    colnames(postprob) = clusterNames
  }

  # Cluster names
  if(is.null(clusterNames)) {
    clusterNames = make.clusterNames(nClusters)
  }
  assert_that(is.character(clusterNames))
  assert_that(length(clusterNames) == nClusters)

  # Trajectories
  assert_that(is.null(trajectories) || is.data.frame(trajectories))

  # Cluster trajectories
  assert_that(is.data.frame(clusterTrajectories) || is.function(clusterTrajectories))
  if(is.function(clusterTrajectories)) {
    # compute cluster trajectories
    center = clusterTrajectories
    rowClusters = clusterAssignments[rleidv(data[[id]])]
    clusterTrajectories = as.data.table(data) %>%
      .[, center(get(response)), by=.(rowClusters, get(time))] %>%
      setnames(c('Cluster', time, response))
  }
  assert_that(has_name(clusterTrajectories, 'Cluster'))
  assert_that(has_name(clusterTrajectories, response))
  assert_that(has_name(clusterTrajectories, time))

  # Converged
  assert_that(is.scalar(converged))
  assert_that(is.logical(converged) || is.numeric(converged) || is.integer(converged))
  assert_that(is.finite(converged))

  # Predict
  assert_that(is.null(predict) || is.function(predict))
  assert_that(is.null(predictPostprob) || is.function(predictPostprob))

  # Create object
  object = .clModelCustom(
               call=call,
               method=method,
               data=data,
               response=response,
               time=time,
               id=id,
               clusterNames=clusterNames,
               clusterAssignments=clusterAssignments,
               clusterTrajectories=clusterTrajectories,
               converged=as.numeric(converged),
               name=name,
               postprob=postprob,
               predict=predict,
               predictPostprob=predictPostprob)
  return(object)
}

#' @export
is.clModelCustom = function(object) {
  is.clModel(object) && is(object, 'clModelCustom')
}

setMethod('getName', signature('clModelCustom'), function(object) object@name)

setMethod('getName0', signature('clModelCustom'), function(object) 'custom')

setMethod('converged', signature('clModelCustom'), function(object) object@converged)

setMethod('postprob', signature('clModelCustom'), function(object) {
  pp = object@postprob
  colnames(pp) = clusterNames(object)
  return(pp)
})


predict.clModelCustom = function(object, newdata=NULL, what='mu', ...) {
  if(is.null(object@predict)) {
    NULL
  } else {
    object@predict(object, newdata, what, ...)
  }
}


#. predictPostprob ####
setMethod('predictPostprob', signature('clModelCustom'), function(object, newdata=NULL, ...) {
  pp = object@predictPostprob(object, newdata, ...)

  assert_that(is.matrix(pp))
  assert_that(nrow(pp) == nIds(object))
  assert_that(!anyNA(pp))
  assert_that(min(pp) >= 0)
  assert_that(max(pp) <= 1)
  assert_that(all(rowSums(pp) == 1))

  colnames(pp) = clusterNames(object)
  return(pp)
})


setMethod('clusterTrajectories', signature('clModelCustom'), function(object, what, at, ...) {
  if(all(at %in% time(object))) {
    dt_traj = object@clusterTrajectories %>%
      as.data.table %>%
      .[, Cluster := factor(Cluster, levels=1:nClusters(object), labels=clusterNames(object))]
  } else if(is.null(object@predict)) {
    stop('predict() not specified for this model')
  } else {
    dt_traj = object@predict(object, at, ...)
  }
  return(dt_traj[])
})

setMethod('trajectories', signature('clModelCustom'), function(object, what, at, ...) {
  if(all(at %in% time(object))) {
    object@trajectories
  } else {
    stop('not supported')
  }
})

#' @export
formula.clModelCustom = function(object, ...) {
  as.formula(paste(object@response, '~ 1'))
}