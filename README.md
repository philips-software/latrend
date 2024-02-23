# latrend
<!-- badges: start -->
  [![R-CMD-check](https://github.com/philips-software/latrend/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/philips-software/latrend/actions/workflows/R-CMD-check.yaml)
  [![CRAN](https://www.r-pkg.org/badges/version/latrend)](https://cran.r-project.org/package=latrend)
  [![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
  [![Codecov test coverage](https://codecov.io/gh/philips-software/latrend/branch/master/graph/badge.svg)](https://codecov.io/gh/philips-software/latrend?branch=master)
  [![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/latrend)](https://www.r-pkg.org/pkg/latrend)
<!-- badges: end -->
The `latrend` package provides a framework for clustering longitudinal datasets in a standardized way. The name is short for _latent-class trend (analysis)_, referring to the discovery of hidden trends in the data. The package provides interfaces to various R packages for conducting this type of analysis.

The package is described in detail in the pre-print available at https://arxiv.org/abs/2402.14621

# Features
* **Unified cluster analysis**, independent of the underlying algorithms used. Enabling users to compare the performance of various longitudinal cluster methods on the case study at hand.
* Supports many different methods for longitudinal clustering out of the box (see the list of supported packages below).
* The framework consists of extensible `S4` methods based on an abstract model class, enabling **rapid prototyping** of new cluster methods or model specifications.
* Standard **plotting** tools for model evaluation across methods (e.g., trajectories, cluster trajectories, model fit, metrics)
* Support for many **cluster metrics** through the packages [clusterCrit](https://CRAN.R-project.org/package=clusterCrit), [mclustcomp](https://CRAN.R-project.org/package=mclustcomp), and [igraph](https://CRAN.R-project.org/package=igraph).
* The structured and unified analysis approach enables **simulation studies** for comparing methods.
* Standardized **model validation** for all methods through bootstrapping or k-fold cross-validation.

See the [release notes page](https://github.com/philips-software/latrend/blob/master/cran-comments.md) for the latest updates.

# Installation
The latest release of _latrend_ can be installed from CRAN by running:
```R
install.packages("latrend")
```

It can also be installed from Github directly using:
```R
remotes::install_github('philips-software/latrend', ref = remotes::github_release())
# include vignettes
remotes::install_github('philips-software/latrend', ref = remotes::github_release(), build_vignettes = TRUE)
```

# Usage
```R
library(latrend)
```
Load and view example data.
```R
data(latrendData)
head(latrendData)
options(latrend.id = "Id", latrend.time = "Time")
plotTrajectories(latrendData, response = "Y")
```
![image](https://user-images.githubusercontent.com/8193083/226573946-70ceb35e-1f31-4c50-b707-2363909c264f.png)

Cluster the trajectories and plot the results.
```R
kmlMethod <- lcMethodKML("Y", nClusters = 3)
model <- latrend(kmlMethod, data = latrendData)
summary(model)
plot(model)
```
![image](https://user-images.githubusercontent.com/8193083/226574139-e41a90f6-c713-4745-980f-b014a42d4aa8.png)


Identify solutions for 1 to 5 clusters.
```R
kmlMethods <- lcMethods(kmlMethod, nClusters = 1:5)
models <- latrendBatch(kmlMethods, data = latrendData)
```

Determine the number of clusters through one or more internal cluser metrics.
```R
metric(models, c("WMAE", "BIC"))
plotMetric(models, c("Dunn", "ASW", "WMAE", "WRSS", "BIC", "estimationTime"))
```
![image](https://user-images.githubusercontent.com/8193083/226574529-befeb9d3-3e3b-44b4-b58c-44295d528358.png)


# Supported packages
The `latrend` package provides interfaces to the relevant methods for longitudinal clustering for the following packages:
* [akmedoids](https://CRAN.R-project.org/package=akmedoids)
* [crimCV](https://CRAN.R-project.org/package=crimCV)
* [dtwclust](https://CRAN.R-project.org/package=dtwclust)
* [fda](https://CRAN.R-project.org/package=fda)
* [flexmix](https://CRAN.R-project.org/package=flexmix)
* [funFEM](https://CRAN.R-project.org/package=funFEM)
* [kml](https://CRAN.R-project.org/package=kml)
* [lcmm](https://CRAN.R-project.org/package=lcmm)
* [mclust](https://CRAN.R-project.org/package=mclust)
* [mixAK](https://CRAN.R-project.org/package=mixAK)
* [mixtools](https://CRAN.R-project.org/package=mixtools)

# Contributing
We appreciate any contributions in the form of ideas, feature requests, bug reports, bug fixes, documentation improvements, code reformatting, and code submissions. Please see the [Contributing guide](https://philips-software.github.io/latrend/CONTRIBUTING.html).
