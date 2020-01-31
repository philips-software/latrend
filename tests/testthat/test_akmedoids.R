context('akmedoids')

library(akmedoids)
data(traj)
capture.output({
  traj = dataImputation(traj, id_field=TRUE, method=2, replace_with=1, fill_zeros=FALSE)
})
trajMat = as.matrix(traj[-1])
rownames(trajMat) = traj[,1]

test_that('default', {
  suppressWarnings({
    model = cluslong(clMethodAKMedoids(), trajMat) %>%
      expect_valid_clModel()
  })
})

test_that('many clusters', {
  suppressWarnings({
    model = cluslong(clMethodAKMedoids(nClusters=10), trajMat) %>%
      expect_valid_clModel()
  })
})