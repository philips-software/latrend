#' @export
#' @name lcModel-make
#' @rdname lcModel-make
#' @title Cluster-handling functions for lcModel implementations.
#' @description Ensures a proper cluster assignments factor vector
#' @param object The `lcModel` object.
#' @param clusters The unprocessed trajectory cluster assignment vector.
#' @return Factor cluster assignments.
#' @keywords internal
make.trajectoryAssignments = function(object, clusters) {
  clusNames = clusterNames(object)
  nClusters = nClusters(object)

  assert_that(!anyNA(clusters))

  if (is.null(clusters)) {
    NULL
  } else if (is.factor(clusters)) {
    # factor
    assert_that(nlevels(clusters) == nClusters)
    if (all(levels(clusters) == clusNames)) {
      clusters
    } else {
      assert_that(all(levels(clusters) %in% clusNames))
      factor(clusters, levels = clusNames)
    }
  } else if (is.integer(clusters)) {
    # integer
    assert_that(min(clusters) >= 1,
      max(clusters) <= nClusters)

    factor(clusters,
      levels = seq_len(nClusters),
      labels = clusNames)
  } else if (is.numeric(clusters)) {
    # numeric
    assert_that(all(vapply(clusters, is.count, FUN.VALUE = FALSE)))
    clusters = as.integer(clusters)
    assert_that(min(clusters) >= 1,
      max(clusters) <= nClusters)

    factor(clusters,
      levels = seq_len(nClusters),
      labels = clusNames)
  } else if (is.character(clusters)) {
    # character
    assert_that(uniqueN(clusters) == nClusters,
      all(clusters %in% clusNames))

    factor(clusters, levels = clusNames)
  } else {
    stop('unsupported clusters input type; expected factor, numeric, or character')
  }
}


#' @export
#' @rdname lcModel-make
#' @title Ensures a proper cluster index vector
#' @return A cluster assignments index vector of type `integer`.
make.clusterIndices = function(object, clusters) {
  clusNames = clusterNames(object)
  nClusters = nClusters(object)

  assert_that(!anyNA(clusters), msg = 'each trajectory should be assigned to a cluster')

  if (is.null(clusters)) {
    NULL
  } else if (is.integer(clusters)) {
    # integer
    assert_that(min(clusters) >= 1,
      max(clusters) <= nClusters)
    clusters
  } else if (is.factor(clusters)) {
    # factor
    if (all(levels(clusters) == clusNames)) {
      as.integer(clusters)
    } else {
      assert_that(all(levels(clusters) %in% clusNames))
      factor(clusters, levels = clusNames) %>%
        as.integer()
    }
  } else if (is.numeric(clusters)) {
    # numeric
    assert_that(all(vapply(clusters, is.count, FUN.VALUE = FALSE)))
    clusters = as.integer(clusters)
    assert_that(min(clusters) >= 1,
      max(clusters) <= nClusters)
    clusters
  } else if (is.character(clusters)) {
    # character
    assert_that(all(clusters %in% clusNames))
    factor(clusters, levels = clusNames) %>%
      as.integer()
  } else {
    stop('unsupported clusters input type; expected factor, numeric, or character')
  }
}


#' @export
#' @rdname lcModel-make
#' @title Generate cluster names
#' @param n The number of clusters.
#' @return A `character` vector length `n` with the cluster names.
make.clusterNames = function(n) {
  assert_that(is.count(n))

  opt = getOption('latrend.clusterNames', LETTERS)

  if (length(opt) == 0) {
    warning('latrend.clusterNames is NULL or empty. Using LETTERS for names')
    clusNames = LETTERS
  } else if (is.function(opt)) {
    clusNames = opt(n) %>% as.character()
  } else {
    clusNames = as.character(opt)
  }

  assert_that(length(clusNames) > 0, anyDuplicated(clusNames) == 0)

  if (n > length(LETTERS)) {
    warning('not enough cluster names provided by latrend.clusterNames')
    clusNames = c(clusNames, paste0('C', seq(length(clusNames) + 1, n)))
  } else {
    clusNames[seq_len(n)]
  }
}
