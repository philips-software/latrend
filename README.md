# latrend
<!-- badges: start -->
  [![R-CMD-check](https://github.com/philips-software/latrend/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/philips-software/latrend/actions/workflows/R-CMD-check.yaml)
  [![CRAN](https://www.r-pkg.org/badges/version/latrend)](https://cran.r-project.org/package=latrend)
  [![Codecov test coverage](https://codecov.io/gh/philips-software/latrend/branch/master/graph/badge.svg)](https://codecov.io/gh/philips-software/latrend?branch=master)
  [![Downloads](https://cranlogs.r-pkg.org/badges/latrend)](https://www.r-pkg.org/pkg/latrend)
<!-- badges: end -->
The `latrend` package provides a framework for clustering longitudinal datasets in a standardized way. It provides interfaces to various R packages for longitudinal clustering.

# Features
* **Unified cluster analysis**, independent of the underlying algorithms used. Enabling users to compare the performance of various longitudinal cluster methods on the case study at hand.
* Supports many different methods for longitudinal clustering out of the box (see the list of supported packages below).
* The framework consists of extensible `S4` methods based on an abstract model class, enabling **rapid prototyping** of new cluster methods or model specifications.
* Standard **plotting** tools for model evaluation across methods (e.g., trajectories, cluster trajectories, model fit, metrics)
* Support for many **cluster metrics** through the packages [clusterCrit](https://CRAN.R-project.org/package=clusterCrit), [mclustcomp](https://CRAN.R-project.org/package=mclustcomp), and [igraph](https://CRAN.R-project.org/package=igraph).
* The structured and unified analysis approach enables **simulation studies** for comparing methods.
* Standardized **model validation** for all methods through bootstrapping or k-fold cross-validation.

See the [news page](https://github.com/philips-software/latrend/blob/master/NEWS.md) for the latest updates.

# Installation
The latest release of _latrend_ can be installed by running:
```
remotes::install_github('philips-software/latrend', ref = remotes::github_release())
```

Vignettes are included by running:
```
remotes::install_github('philips-software/latrend', ref = remotes::github_release(), build_vignettes = TRUE)
```

# Usage
```
library(latrend)
```
Load and view example data.
```
data(latrendData)
head(latrendData)
options(latrend.id = "Id", latrend.time = "Time")
plotTrajectories(latrendData, response = "Y")
```
Cluster the trajectories and plot the results.
```
kmlMethod <- lcMethodKML("Y", nClusters = 3)
model <- latrend(kmlMethod, data = latrendData)
summary(model)
plot(model)
```

Identify solutions for 1 to 5 clusters.
```
kmlMethods <- lcMethods(kmlMethod, nClusters = 1:5)
models <- latrendBatch(kmlMethods, data = latrendData)
```

Determine the number of clusters through one or more internal cluser metrics.
```
metric(models, c("WMAE", "BIC"))
plotMetric(models, c("WMAE", "BIC"))
```

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
* [longclust](https://CRAN.R-project.org/package=longclust)
* [mclust](https://CRAN.R-project.org/package=mclust)
* [mixAK](https://CRAN.R-project.org/package=mixAK)
* [mixtools](https://CRAN.R-project.org/package=mixtools)
