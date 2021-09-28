**If you are viewing this file as part of a released version of the package, the latest news can be found [on GitHub](https://github.com/philips-software/latrend/blob/master/NEWS.md).**

# latrend [v1.2.0](https://github.com/philips-software/latrend/milestone/2) (in development)

## New features
1. Greatly expanded documentation of `lcMethod`, `lcModel`, `transformFitted()`, and `transformPredict()`.
2. `latrendBatch()` evaluates and validates methods and datasets prior to any fitting ([#49](https://github.com/philips-software/latrend/issues/49)). This informs users of any errors as soon as possible. Moreover it makes it significantly easier to run parallel computations without the need to export parts of the global environment.
3. Added `seed` argument to `latrendBatch()` ([#47](https://github.com/philips-software/latrend/issues/47)). Similar to `latrendRep()`, seeds are now generated for all methods, allowing for reproducible results.
4. `latrendBatch()` now supports an expression for its `"data"` argument ([#50](https://github.com/philips-software/latrend/issues/50)).
5. Variable argument pass-through for `lcModel` methods.
6. Default implementation for `predictForCluster()`.
7. Added timing information to log output of `latrend*()` methods ([#51](https://github.com/philips-software/latrend/issues/51)).
8. Added `"unit"` option to `estimationTime()`.
9. Implemented `estimationTime()` for `lcModels`.
10. `plot(lcModel)` only shows trajectories when `"what"` argument is not specified.
11. Support for `lcModel` objects without training data ([#36](https://github.com/philips-software/latrend/issues/36)).
12. Made it easier to define new `lcMethod` subclasses by defining better default methods.
13. `plot()` for `lcModels` ([#48](https://github.com/philips-software/latrend/issues/48))
14. `latrend()` and derivative methods automatically supress console output when `verbose = FALSE` ([#45](https://github.com/philips-software/latrend/issues/45))
15. Better automatic axis breaks in metric plots (#44).
16. `trajectoryAssignments()` signature that accepts a posterior probability matrix ([#34](https://github.com/philips-software/latrend/issues/34))
17. Added convenient mixture initialization options to `lcMethodLcmmGBTM` and `lcMethodLcmmGMM` based on standard (single cluster) linear mixed model fit.
18. `lcMethodRandom()` accepts `seed` argument.
19. Expand trajectory assignment input options for `lcModelPartition`.

## Breaking changes
1. Significant: `trajectories()` now returns the original training data, instead of the fitted (predicted) data ([#32](https://github.com/philips-software/latrend/issues/32)). This was done to improve clarity. 
Previous uses of `trajectories()` and `plotTrajectories()` should be replaced by `fittedTrajectories()` and `plotFittedTrajectories()`, respectively.
2. Minor: `lcMethod` implementations: `prepareData()` must now return an `environment` ([#39](https://github.com/philips-software/latrend/issues/39)). In the past, `NULL` was allowed, but this increased code complexity further down the process.
3. Minor: `estimationTime()` is now an S4 generic method. This does not affect existing code.
 
## Bug fixes
1. Critical: Fixed `predict()` when cluster membership is specified for the new data ([#40](https://github.com/philips-software/latrend/issues/40)).
2. Critical: Fixed computation of WMAE and WMSE metrics ([#52](https://github.com/philips-software/latrend/issues/52)).
3. Fixed `lcMethod` argument evaluation for symbolic name input that equals the respective argument name ([#41](https://github.com/philips-software/latrend/issues/41)).
4. Fixed `strip()` error related to use of `eapply()`.
5. Fixed output error for `latrendBatch()` when `errorHandling = "pass"` ([#46](https://github.com/philips-software/latrend/issues/46))
6. Defined estimation time for `lcModelPartition` and `lcModelWeightedPartition` ([#38](https://github.com/philips-software/latrend/issues/38)).
7. Fixed default output of `logLik.lcModel` and other implementations ([#37](https://github.com/philips-software/latrend/issues/37)).
8. Default `metric()` did not compute any metrics.
9. Fixed indentation of messages for `latrend()` and derivative methods.