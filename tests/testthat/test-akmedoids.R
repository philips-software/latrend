context('akmedoids')
skip_if_not_installed('akmedoids')
rngReset()

library(akmedoids)
data(traj)
capture.output({
  traj = dataImputation(traj, id_field=TRUE, method=2, replace_with=1, fill_zeros=FALSE)
})
trajMat = as.matrix(traj[-1])
rownames(trajMat) = traj[,1]

test_that('default', {
  suppressWarnings({
    model = latrend(lcMethodAkmedoids(response = 'Value'), trajMat)
  })
  expect_valid_lcModel(model)
})

test_that('many clusters', {
  suppressWarnings({
    model = latrend(lcMethodAkmedoids(response = 'Value', nClusters=10), trajMat)
  })
  expect_valid_lcModel(model)
})
