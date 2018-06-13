TVEMMixNormal <- function( dep,   # The dependent variable as a vector, one
                  # entry per assessment per subject
           id,      # The subject ID as a vector, one
                  # entry per assessment per subject
                  # (could be integers or strings)
           numInteriorKnots,   # The number of interior knots
                     # for the splines representing
                     # the time-varying coefficients.
                     # Assumed to be the same
                     # for each coefficient.
           numClasses,  # Number of classes in the mixture model
           tcov,      # Time-varying covariates as a matrix, one
                  # row per assessment per person.  Usually,
                  # the first column should be a column of
                  # ones representing an intercept.
           time,      # Assessment time as a vector
   # These arguments have default values, given after the equal sign for each:
           assumeIndependence=FALSE, # If this is true then the
                  # autocorrelation parameter will be treated as zero.
           convergenceCriterion=1e-6,  # Convergence criterion for the
                  # maximum absolute deviation of parameter
                  # estimates between successive
                  # steps of the EM algorithm
           deg=3,     # Degree of the polynomial between
                  # successive knots for the time-
                  # varying coefficient functions.
           doPlot=TRUE,   # Whether to draw a plot of the time-
                  # varying coefficient functions.
           getSEs=TRUE,   # Whether to calculate standard errors.
                  # Setting this to FALSE would save
                  # computational time.
           gridSize=1000, # The number of values of time for which
                  # to obtain estimates of the coefficient
                  # functions
           maxIterations=1000, # Maximum number of EM iterations
                  # to attempt;
           maxVarianceRatio=10, # Maximum ratio between estimated
                  # variances of different classes;
           min.time=NA,   # beginning of interval of interest; if left
                          # at NA, it will default to min(time) where
                          # time is the time variable given above
           max.time=NA,   # end of interval of interest; if left
                          # at NA, it will default to max(time) where
                          # time is the time variable given above
           numStarts=50,  # Number of random starting values to use.;
           referenceClass=1,  # Reference class.  If it is not
                  # specified here, then the first class
                  # will be selected as the reference class.
           seed=NA,     # The initial random seed.  It will be used
                  # to fit the model (if numStarts=1) or to
                  # generate more seeds (if numStarts>1).
           scov=NULL,   # Subject-level class membership prediction
                  # covariates, as a matrix.  A column of 1's
                  # should not be provided for scov because,
                  # unlike in the case of tcov, an intercept
                  # column is included automatically
                  # by the code.  This should have the same
                  # number of rows as tcov, but should be
                  # identical on all observations within the
                  # same person.
           useAIC=FALSE, # If true, uses AIC for tuning instead of BIC.
           xcov=NULL    # Optional matrix of covariates assumed
                  # to have time-invariant effects.
           ) {
  ##################################################################
  # MixTVEM macro Version 1.1
  # By John DZIAK, Xianming TAN, and Runze LI
  # Fits a mixture of nonparametric trajectories to longitudinal data.
  #
  # Copyright:
  # (c) 2016 The Pennsylvania State University
  #
  # License:
  # This program is free software; you can redistribute it and/or
  # modify it under the terms of the GNU General Public License as
  # published by the Free Software Foundation; either version 2 of
  # the License, or (at your option) any later version.
  #
  # This program is distributed in the hope that it will be useful,
  # but WITHOUT ANY WARRANTY; without even the implied warranty of
  # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  # General Public License for more details.
  #
  # Acknowledgments and references:
  # We fit a mixture of nonparametric varying-coefficient models, using
  # a penalized B-spline approach.  See
  #  Eilers, P.H.C. and Marx, B.D. (1996). Flexible smoothing using
  #    B-splines and penalized likelihood. Statistical Science
  #    11(2): 89-121.
  #  Hastie, T., & Tibshirani, R. (1993). Varying-coefficient models.
  #    Journal of the Royal Statistical Society, Series B, 55, 757-796.
  #  Shiyko, M. P., Lanza, S. T., Tan, X., Li, R., Shiffman, S. (2012).
  #    Using the Time-Varying Effect Model (TVEM) to Examine Dynamic
  #    Associations between Negative Affect and Self Confidence on Smoking :
  #    Urges Differences between Successful Quitters and Relapsers.
  #    Prevention Science, 13, 288-299.
  #  Ramsay, J., Hooker, G., & Graves, S. (2009). Functional Data Analysis
  #    with R and MATLAB. New York: Springer.
  #  Tan, X., Shiyko, M. P., Li, R., Li, Y., & Dierker, L. (2011, November 21).
  #    A time-varying effect model for intensive longitudinal data.
  #    Psychological Methods. Advance online publication.
  #    doi: 10.1037/a0025814.
  # Estimation is done using the EM algorithm for finite mixtures.
  #  McLachlan, G. J., and Peel, D. (2000). Finite mixture models. New York:
  #     Wiley.
  #  Dempster, A. P., Laird, N. M., and Rubin, D. B. (1977). Maximum
  #    likelihood from incomplete data via the EM algorithm. Journal of
  #    the Royal Statistical Society, B, 39, 1-38.
  # The standard error calculations for the mixture approach are based on
  # those used by Turner (2000) and Turner's mixreg R package, which are
  # based on the ideas of Louis (1982).
  #  Louis, T. A. (1982). Finding the Observed Information Matrix when Using
  #    the EM Algorithm. Journal of the Royal Statistical Society, B, 44,
  #    226-233.
  #  Turner, T. R. (2000) Estimating the rate of spread of a viral infection
  #    of potato plants via mixtures of regressions. Applied Statistics,
  #    49, pp. 371-384.
  #  Turner, R. (2009). mixreg: Functions to fit mixtures of regressions.
  #    R package version 0.0-3. http://CRAN.R-project.org/package=mixreg
  # The use of a sandwich formula to adjust for within-subject correlation
  # when calculating the standard errors is inspired by
  #  Liang, K.-Y., and Zeger, S. L. (1986). Longitudinal data analysis
  #    using generalized linear models. Biometrika, 73, 13-22.
  # Clustering functional data modeled with splines is described in
  #  James, G., and Sugar, C. (2003) Clustering for sparsely sampled
  #    functional data.  Journal of the American Statistical
  #    Association 98, 397-408.
  # The model fit criteria used are adapted versions of the standard AIC, BIC
  # and GCV of:
  #  Akaike, H. (1973). Information theory and an extension of the maximum
  #     likelihood principle. In B. N. Petrov & F. Csaki (Eds.), Second
  #     international symposium on information theory (p. 267-281).
  #     Budapest, Hungary: Akademai Kiado.
  #   Schwarz, G. (1978). Estimating the dimension of a model. Annals of
  #     Statistics, 6, 461-464.
  #   Craven, P., and Wahba, G. (1978). Smoothing noisy data with spline
  #     functions: Estimating the correct degree of smoothing by the
  #     method of generalized cross-validation. Numerische
  #     Mathematik, 31, 377?403.

  ## Define required helper functions:
      MixTVEMFitInner <- function(convergenceCriterion,
                  intId,
                  maxIterations=1000,
                  maxVarianceRatio,
                  proportionNugget=.5,
                  numClasses,
                  penaltyMatrix=NULL,
                  referenceClass,
                  rho=0,
                  roughnessPenalty=0,
                  S=S,
                  time,
                  X,
                  Y  ) {
       ## Prepare to begin loop;
         stopifnot(length(time)==length(intId));
         theta <- matrix(0,ncol(X),numClasses);
         oldGamma <- Inf;
         oldtheta <- Inf;
         maxAbsDev <- Inf;
         numSubjects <- max(intId);
         numTotal <- length(intId);
         nonreferenceClasses <- (1:numClasses)[-referenceClass];
         iteration <- 1;
         numObsBySub <- table(intId); # assumes that intId
             # consists of consecutive integers starting at 1;
         stopifnot(length(Y)==numTotal);
         stopifnot(length(unique(intId))==numSubjects);
         stopifnot(all.equal(unique(intId),1:numSubjects));
         stopifnot(as.integer(rownames(numObsBySub))==1:numSubjects);
         stopifnot(identical(intId,sort(intId)));
         stopifnot(length(numObsBySub)==numSubjects);
         stopifnot(length(intId)==numTotal);
         stopifnot(identical(intId,sort(intId)));
       ## Initial E step (generate random posterior probabilities;
         temp <- matrix(rexp(numSubjects*numClasses),
                   numSubjects,
                   numClasses);
         postProbsBySub <- temp/apply(temp,1,sum);
       ## Calculate inverse covariance matrices;
       covMats <- list();
       invCovMats <- list();
       dets <- list();
       sigsq.total <- rep(1,numClasses);  # starting value;
       for (i in 1:numSubjects) {
          these <- which(intId==i);
          covMats[[i]] <- (1 - proportionNugget) *
                  rho^(abs(outer(time[these],time[these],"-"))) +
                  proportionNugget*diag(length(these));
          invCovMats[[i]] <- solve(covMats[[i]]);
          dets[[i]] <- det(covMats[[i]]);
       }
       ## Begin loop;
         while ((iteration<maxIterations)&
             (maxAbsDev>convergenceCriterion)) {
          ## Initial work;
            iteration <- iteration+1;
            postProbsByAssessment <- matrix(0,numTotal,numClasses);
            for (k in 1:numClasses) {
             postProbsByAssessment[,k] <-
                 rep(postProbsBySub[,k],numObsBySub);
            }
          ## M Step;
              ## Get new beta estimates;
              modelList <- list();
              for (k in 1:numClasses) {
               denominator <- matrix(0,ncol(X),ncol(X));
               numerator <- matrix(0,ncol(X),1);
               for (i in 1:numSubjects) {
                  these <- which(intId==i);
                  denominator <- denominator +
                     t(X[these,,drop=FALSE])%*%(invCovMats[[i]]/sigsq.total[k])%*%
                        (postProbsBySub[i,k]*X[these,,drop=FALSE]);
                  numerator <- numerator + t(X[these,,drop=FALSE])%*%
                       (invCovMats[[i]]/sigsq.total[k])%*%
                         (postProbsBySub[i,k]*Y[these]);
               }
               theta[,k] <- solve(denominator + # negative Hessian as usual
                  roughnessPenalty*penaltyMatrix, #+ # second order difference penalty
                  #(1e-6)*roughnessPenalty*diag(as.integer(ncol(penaltyMatrix))), # small additional ridge penalty;
                  numerator);
              }
              fittedY <- X%*%theta; # one column for each class;
              stopifnot(nrow(postProbsByAssessment)==nrow(fittedY));
              stopifnot(length(Y)==nrow(fittedY));
              residsY <- kronecker(t(rep(1,numClasses)),Y)-fittedY;
           ## Get new sigma estimates;
              f <- function(i) {
              return(apply(residsY[which(intId==i),,drop=FALSE]^2,
                  2,sum))};
              rssBySubjectAndClass <- t(sapply(1:numSubjects,f));
              if (nrow(rssBySubjectAndClass)==1)
                {rssBySubjectAndClass <- t(rssBySubjectAndClass);}
                    # corrects for the unusual fact that
                    # the result from sapply seems to come out
                    # transposed when there is only 1 column,
                    # relative to when there is >1;
               for (k in 1:numClasses) {
                sigsq.total[k] <- sum(rssBySubjectAndClass[,k]*
                            postProbsBySub[,k]) /
                      sum(numObsBySub*postProbsBySub[,k]);
              }
              if (!is.na(maxVarianceRatio)) {
                if (max(sigsq.total)/min(sigsq.total)>
                      maxVarianceRatio) {
                 sigsq.total[which(sigsq.total<(max(sigsq.total)
                      /maxVarianceRatio))] <-
                       max(sigsq.total)/maxVarianceRatio;
                }
              }
            ## Get new gamma estimates;
              SExtended <- kronecker(S,rep(1,numClasses));
              colnames(SExtended) <- colnames(S);
              classForLR <- rep(1:numClasses,times=numSubjects);
              weightForLR <- as.vector(t(postProbsBySub));
              dataForLR <- cbind(rep(1:numSubjects,each=numClasses),
                         classForLR,
                         weightForLR,
                         SExtended);
              numGamma <- ncol(SExtended);
              gamma <- matrix(0,numGamma,numClasses);
              eta <- matrix(0,numSubjects,numClasses);
              for (k in nonreferenceClasses) {
               outcomeForLogisticRegression <- 1*(classForLR==k);
               outcomeForLogisticRegression[(classForLR!=k)&
                     (classForLR!=referenceClass)] <- NA;
               warnOption <- getOption("warn");
               options(warn=-1);
               thisModel <- glm(outcomeForLogisticRegression~
                               SExtended+0,
                        family=binomial,
                        weights=weightForLR);
               options(warn=warnOption);
               gamma[,k] <- thisModel$coef;
               eta[,k] <- S%*%gamma[,k];
              }
              fittedProb <- exp(eta)/apply(exp(eta),1,sum);
          ## E Step (get new likelihood contributions
          ## and posterior probabilities);
            logProbability <- matrix(0,numSubjects,numClasses);
            wrssBySubjectAndClass <- matrix(0,numSubjects,numClasses);
            for (k in 1:numClasses) {
              f <- function(i,k) {
                 these <- which(intId==i);
                 return( drop(t(residsY[these,k,drop=FALSE])%*%
                    (invCovMats[[i]]/sigsq.total[k])%*%residsY[these,k,drop=FALSE]) );
              };
              wrssBySubjectAndClass[,k] <- drop(sapply(1:numSubjects,f,k));
              f <- function(i,k) {
                  these <- which(intId==i);
                  return( (sigsq.total[k]^length(these))*dets[[i]] );
              };
              determinantsBySubject.class.k <-
                                 drop(t(sapply(1:numSubjects,f,k)));
              if (sum(is.nan(log(determinantsBySubject.class.k+1e-30)))>0) {
                warning("Problem with covariance structure estimation");
                print("log determinant NaN");
                print("sigsq.total:");print(sigsq.total);
                print("rho:");print(rho);
              }
              logProbability[,k] <- -(numObsBySub/2)*log(2*3.1415926535)-
                     (1/2)*(log(determinantsBySubject.class.k+1e-30)) -
                                  wrssBySubjectAndClass[,k]/2;
            }
            tempMatrix <- logProbability;
            for (k in 1:numClasses) {
             tempMatrix[,k] = tempMatrix[,k] + log(fittedProb[,k]+1e-30);
            }
            logLikelihoodBySubject <- log(apply(exp(tempMatrix),1,sum));
            logLik <- sum(logLikelihoodBySubject);
            tempMax <- apply(tempMatrix,1,max);
            for (k in 1:numClasses) {
             tempMatrix[,k] <- tempMatrix[,k] - tempMax;
            }
            expTempMatrix <- exp(tempMatrix);
            for (k in 1:numClasses) {
             postProbsBySub[,k] <- expTempMatrix[,k]/apply(expTempMatrix,1,sum);
            }
            maxAbsDev <- max(abs(c(as.vector(theta)-oldtheta,
                       as.vector(gamma)-oldGamma)));
            oldGamma <- as.vector(gamma);
            oldtheta <- as.vector(theta);
         }
         ## Done with main loop
         enpByClass <- rep(0,numClasses);
         for (k in 1:numClasses) {
           denominator <- matrix(0,ncol(X),ncol(X));
           for (i in 1:numSubjects) {
             these <- which(intId==i);
             denominator <- denominator + postProbsBySub[i,k]*t(X[these,,drop=FALSE])%*%
                             (invCovMats[[i]]/sigsq.total[k])%*%X[these,,drop=FALSE];
           }
           for (i in 1:numSubjects) {
             these <- which(intId==i);
             enpByClass[k] <- enpByClass[k] + postProbsBySub[i,k]*sum(diag(as.matrix((X[these,,drop=FALSE]%*%
                    solve(denominator +
                         roughnessPenalty*penaltyMatrix,# +
                        # (1e-6)*roughnessPenalty*diag(as.integer(ncol(penaltyMatrix))),
                        (t(X[these,,drop=FALSE])%*%(invCovMats[[i]]/sigsq.total[k])))))));
           }
         }
         if (assumeIndependence==TRUE) { # this is a rather sloppy call to a global variable;
	         enp <- sum(enpByClass) +
	            ncol(S)*(numClasses-1)+
	             numClasses ;
	         np <- ncol(X)*numClasses + ncol(S)*(numClasses-1)+
	            numClasses  ;
         } else {
	         enp <- sum(enpByClass) +
	            ncol(S)*(numClasses-1)+
	             numClasses +2;
	         np <- ncol(X)*numClasses + ncol(S)*(numClasses-1)+
	            numClasses  +2;
         }
         converged <- maxAbsDev<=convergenceCriterion;
         weightedRSS <- sum(postProbsBySub*rssBySubjectAndClass);
         return(list(aic=-2*logLik+2*enp,
                  # A statistic similar to AIC (Akaike, 1973)
                  # but using the effective (penalized) number of
                  # parameters instead of a count of parameters
                  # (see Eilers and Marx, 1996)
               bic=-2*logLik+log(numSubjects)*enp,
                  # A statistic somewhat analogous to BIC
                  # (Schwarz, 1978).  The effective (penalized)
                  # number of parameters is used instead of a
                  # count of parameters (see Eilers and Marx,
                  # 1996).
               converged=converged,
                  # Indicates whether the EM algorithm converged
               dep=Y,
                  # The observed response for each assessment,
                  # copied here for convenience when doing
                  # fit diagnostics, follow-up analyses, etc.
               enp=enp,
                  # The effective number of regression
                  # parameters (see Eilers and Marx, 1996)
                  # per class plus the number of free
                  # gamma parameters plus the number of free
                  # sigma parameters, summed across classes.
               fittedProb=fittedProb,
                  # The fitted values for class probabilities
                  # given the S covariates for each subject,
                  # as given by the logistic regression model
                  # for class membership.
               fittedY=fittedY,
                  # The fitted values for the dependent
                  # variable at each assessment.
               gamma=gamma,
                  # The estimated logistic regression
                  # parameters in the model for class
                  # membership.
               intId=intId,
                  # The internally assigned subject identification
                  # number for each assessment,
                  # copied here for convenience when doing
                  # fit diagnostics, follow-up analyses, etc.
               iteration=iteration,
                  # The number of iterations run by the EM
                  # algorithm.
               lambda=roughnessPenalty,
                  # The current candidate value of the penalty
                  # weight for the roughness penalty
               logLik=logLik,
                  # The fitted log-likelihood
               np=np,
                  # The counted number of parameters,
                  # ignoring shrinkage imposed by the penalty
               postProbsBySub=postProbsBySub,
                  # Each subject's estimated posterior
                  # probability of belonging to each class.
               postProbsByAssessment=postProbsByAssessment,
                  # The same as postProbsBySub, except that
                  # they are repeated within subject so that
                  # there is one row per assessment rather than
                  # only one row per subject.
               proportionNugget=proportionNugget,
                  # The value assumed for the within-subject
                  # proportion of error variance which is
                  # idiosyncratic to a single time point
               residsY = residsY,
                  # Residuals for each observation under the
                  # model for each class;
               rho = rho,
                  # The value assumed for the autocorrelation
                  # parameter;
               sigsq.total=sigsq.total,
                  # Estimated sigma squared (variance)
                  # parameters for each class.
               theta=theta,
                  # Estimated regression coefficients for each
                  # of the regression coefficients, including
                  # spline basis coefficients
               weightedGCV=weightedRSS/(numTotal*((1-(enp/numTotal))**2)),
                  # A statistic similar to GCV (see Wahba, 1990;
                  # Eilers and Marx, 1996) but calculated
                  # with the weightedRSS instead of an ordinary
                  # residual sum of squares.
               weightedRSS=weightedRSS
                  # The weighted residual sum of squares
                  # error measure.  It is like the classic
                  # regression sum of squared residuals
                  # (sum(y-yhat)^2), but it is weighted
                  # by the posterior probabilities estimated
                  # for each individual and class.
            ));
    }
    estimateRho <- function (thisFit, time) {
      ## Get new negLogRho estimate;
      sigsq.total <- thisFit$sigsq.total;
      numClasses <- ncol(thisFit$postProbsBySub);
      crossprods <- NULL;
      lags <- NULL;
      weights<- NULL;
      for (k in 1:numClasses) {
        for (i in unique(intId)) {
          these <- which(intId==i);
          temp1 <- outer(thisFit$residsY[these,k],thisFit$residsY[these,k],"*");
          new.crossprods <- as.vector(temp1[upper.tri(temp1,diag=FALSE)] ) / sigsq.total[k];
          crossprods <- c(crossprods,new.crossprods);
          temp1 <- abs(outer(time[these],time[these],"-"));
          new.lags <- as.vector(temp1[upper.tri(temp1,diag=FALSE)]);
          lags <- c(lags,new.lags);
          stopifnot(length(new.crossprods)==length(new.lags));
          weights<- c(weights,
               rep(thisFit$postProbsBySub[i,k],length(new.crossprods)));
        }
      }
      initial.proportionNugget <- 1 / 2;
      average.cross.product <- weighted.mean(x=crossprods, w=weights);
      average.lag <- weighted.mean(x=lags, w=weights);
      if (average.cross.product <= 0) {
          proportionNugget <- .9999999;
          rho <- 1e-10;
      } else {
          initial.rho <-exp((log(average.cross.product) - log(1 - initial.proportionNugget))/average.lag);
          y.for.nlsfit <- crossprods;
          x.for.nlsfit <- lags;
          nlsfit1 <- nls(y.for.nlsfit ~ pnn*exp(logrho*x.for.nlsfit),
                   weights=weights,
                   control=nls.control(maxiter = 500, printEval=FALSE, warnOnly = TRUE),
                   start=list(logrho=log(initial.rho), pnn=1-initial.proportionNugget));
          rho <- exp(summary(nlsfit1)$coefficients["logrho","Estimate"]);
          proportionNugget <- 1-summary(nlsfit1)$coefficients["pnn","Estimate"];
      }
      return(list(rho=rho,proportionNugget =proportionNugget));
    }
    MixTVEMCovMats <- function(  fittedProb,
                   gamma,
                   intId,
                   mu,
                   numSubjects,
                   numTotal,
                   penalty,
                   postProb,
                   proportionNugget,
                   referenceClass,
                   rho,
                   S,
                   sigsq.total,
                   theta,
                   X,
                   Y) {
      numthetas <- nrow(theta);
      numClasses <- ncol(theta);
      stopifnot(numthetas==ncol(X));
      numGammas <- nrow(gamma);
      stopifnot(numClasses==ncol(mu));
      nonreferenceClasses <- (1:numClasses)[-referenceClass];
      numParams <- (numClasses*numthetas) +
               (numClasses-1)*numGammas;
      thetaIndex <- matrix(0,numthetas,numClasses);
      if (numClasses>1) {
        gammaIndex <- matrix(0,numGammas,numClasses-1);
      } else {gammaIndex <- NULL;}
      for (k in 1:numClasses) {
         thetaIndex[,k] <- (((k-1)*numthetas+(k-1)*numGammas) +
                   (1:numthetas));
         if (k < numClasses) {
           gammaIndex[,k] <- (k*numthetas+(k-1)*numGammas) +
                       (1:numGammas);
         }
      }
      y.inv.corrmats <- list();
      for (i in 1:numSubjects) {
         y.inv.corrmats[[i]] <- solve((1 - proportionNugget)*rho^(abs(outer(time[which(intId==i)],
                                                 time[which(intId==i)],"-")))
                                     + proportionNugget*diag(length(which(intId==i))));
      }
      # These calculations are based on the mixreg package in Turner
      # (2009) with some modifications as described in our paper;
      I1 <- matrix(0,numParams,numParams);
      I2 <- matrix(0,numParams,numParams);
      I3 <- matrix(0,numParams,numParams);
      # Calculate matrix I1
      for (c in 1:numClasses) {
         for (i in 1:numSubjects) {
           these <- which(intId==i);
           xi <- X[which(intId==i),,drop=FALSE];
           ni <- sum(intId==i);
               contrib <- postProb[i,c]*
                    t(as.matrix(xi))%*%(y.inv.corrmats[[i]]/sigsq.total[c])%*%as.matrix(xi);
           I1[thetaIndex[,c],thetaIndex[,c]] <-
                 I1[thetaIndex[,c],thetaIndex[,c]] + contrib;
         }
         I1[thetaIndex[,c],thetaIndex[,c]] <- I1[thetaIndex[,c],thetaIndex[,c]] + penalty;
      }
      if (numClasses>1) {
         for (c in 1:(numClasses-1)) {
          for (k in 1:(numClasses-1)) {
           for (i in 1:numSubjects) {
            nonrefc <- nonreferenceClasses[c];
            nonrefk <- nonreferenceClasses[k];
            Si <- S[i,,drop=FALSE];
            contrib <- fittedProb[i,nonrefc]*
                  (1*(nonrefc==nonrefk)-fittedProb[i,nonrefk])*
                   crossprod(Si);
            I1[gammaIndex[,c],gammaIndex[,k]] <-
                  I1[gammaIndex[,c],gammaIndex[,k]] + contrib;
           }
          }
         }
      }
      # Calculate matrix I2 (information lost due to not knowing true class labels)
      for (c in 1:numClasses) {
         for (k in 1:numClasses) {
          for (i in 1:numSubjects) {
           sigsqS <- sigsq.total[c]*(1 - proportionNugget);
           sigsqE <- sigsq.total[c]*proportionNugget;
           ni <- sum(intId==i);
           xi <- X[which(intId==i),,drop=FALSE];
           yi <- Y[which(intId==i),drop=FALSE];
           Si <- S[i,,drop=FALSE];
           muic <- mu[which(intId==i),c,drop=FALSE];
           hic <- t(xi)%*%(y.inv.corrmats[[i]]/sigsq.total[c])%*%(yi-muic);
           for (j in 1:numSubjects) {
             # Information loss in linear regression coefficients:
             nj <- sum(intId==j);
             xj <- X[which(intId==j),,drop=FALSE];
             yj <- Y[which(intId==j),drop=FALSE];
             Sj <- S[j,,drop=FALSE];
             mujk <- mu[which(intId==j),k,drop=FALSE];
             hjk <- t(xj)%*%(y.inv.corrmats[[j]]/sigsq.total[k])%*%(yj-mujk);
             Gckij <- postProb[i,c]*postProb[j,k]*(i!=j) +
                  postProb[i,c]*(i==j)*(c==k);
             I2[thetaIndex[,c],thetaIndex[,k]] <-
                   I2[thetaIndex[,c],thetaIndex[,k]] +
                     Gckij * hic%*%t(hjk) -
                     (1/(numSubjects^2))*postProb[i,c]*hic%*%t(theta[,k])%*%penalty -
                     (1/(numSubjects^2))*postProb[j,k]*hjk%*%t(theta[,c])%*%penalty +
                     (1/(numSubjects^2))*penalty%*%theta[,c]%*%t(theta[,k])%*%penalty;
             # Information loss in logistic regression coefficients:
             if ((c < numClasses)&(k < numClasses)) {
                nonrefc <- nonreferenceClasses[c];
                nonrefk <- nonreferenceClasses[k];
                Gcnonrefkij <- postProb[i,c]*postProb[j,nonrefk]*(i!=j) +
                    postProb[i,c]*(i==j)*(c==nonrefk);
                Gnonrefcnonrefkij <- postProb[i,nonrefc]*postProb[j,nonrefk]*(i!=j) +
                   postProb[i,nonrefc]*(i==j)*(nonrefc==nonrefk);
                contrib <- (Gnonrefcnonrefkij-
                          postProb[i,nonrefc]*fittedProb[j,nonrefk]-
                          postProb[j,nonrefk]*fittedProb[i,nonrefc] +
                          fittedProb[i,nonrefc]*fittedProb[j,nonrefk])*
                            (t(Si)%*%Sj);
                I2[gammaIndex[,c],gammaIndex[,k]] <-
                     I2[gammaIndex[,c],gammaIndex[,k]] +
                       contrib;
            }
            # Information loss in covariance between linear and
            # logistic regression coefficients:
             if (k < numClasses) {
               for (kprime in 1:numClasses) {
                 nonrefk <- nonreferenceClasses[k];
                 Gcnonrefkij <- postProb[i,c]*postProb[j,nonrefk]+
                       postProb[i,c]*(i==j)*
                       ((c==nonrefk)-postProb[j,nonrefk]);
                 contrib = Gcnonrefkij*((k==kprime)-fittedProb[j,k])*hic%*%Sj +
                           ( (1/numSubjects)*postProb[j,kprime] *
                             ((k==kprime)-fittedProb[j,k])*penalty%*%theta[,c]%*%Sj) ;
                 I2[thetaIndex[,c],gammaIndex[,k]] <-
                       I2[thetaIndex[,c],gammaIndex[,k]] +
                      contrib;
                 I2[gammaIndex[,k],thetaIndex[,c]] <-
                       t(I2[thetaIndex[,c],gammaIndex[,k]]);
               }
            }
           }
          }
         }
      }
      # Calculate matrix I3
      for (i in 1:numSubjects) {
         scorei <- matrix(0,numParams,1);
         for (k in 1:numClasses) {
           ni <- sum(intId==i);
           xi <- X[which(intId==i),,drop=FALSE];
           yi <- Y[which(intId==i),drop=FALSE];
           Si <- S[i,,drop=FALSE];
           muik <- mu[which(intId==i),k,drop=FALSE];
           hik <- t(xi)%*%(y.inv.corrmats[[i]]/sigsq.total[k])%*%(yi-muik);
           scorei[thetaIndex[,k]] <- postProb[i,k]*hik -
                     (1/numSubjects)*penalty%*%theta[,k];
           if (k < numClasses) {
             nonrefk <- nonreferenceClasses[k];
             scorei[gammaIndex[,k]] <- (postProb[i,nonrefk]-
                        fittedProb[i,nonrefk])*Si;
           }
         }
         I3 <- I3 + crossprod(t(scorei));
      }
      # Calculate naiveCovarianceMatrix (in theta and gamma)
      if (kappa(I1)<Inf) {
         uncorrectedCovarianceThetaGamma <- solve(I1)%*%I3%*%solve(I1);
      } else {
         uncorrectedCovarianceThetaGamma <- matrix(NA,nrow(I1),ncol(I1));
      }
      if (kappa(I1-I2)<Inf) {
         correctedCovarianceThetaGamma <- solve(I1-I2);
      } else {
         correctedCovarianceThetaGamma <- matrix(NA,nrow(I1),ncol(I1));
      }
      # Calculate covarianceMatrix (in theta and gamma)
      if (det(I3)>0) {
         sandwichCovarianceThetaGamma <- correctedCovarianceThetaGamma%*%
                    I3%*%correctedCovarianceThetaGamma;
      }
      # Calculate covarianceForPrevalences
      if (numClasses>1) {
         allGammas <- as.vector(gammaIndex);
         jacobian <- matrix(0,numClasses,(numClasses-1)*numGammas);
         for (m in 1:(numClasses-1)) {
           for (k in 1:numClasses) {
             for (q in 1:numGammas) {
             w <- (m-1)*numGammas + q;
             nonrefm <- nonreferenceClasses[m];
             # Jacobian[m,i] = derivative of lambda[k] in gamma[m,q], ;
             #   i.e., gamma[w] when the free gammas are all listed in a ;
             #   vector, one class on top of another, excluding the
             #   reference class. ;
             temp <- sum(((1*(k==nonrefm))-fittedProb[,k])*
                   fittedProb[,nonrefm]*S[,q]);
             jacobian[k,w] <- temp/numSubjects;
              }
           }
         }
        uncorrectedCovariancePrevalence <- jacobian %*%
             uncorrectedCovarianceThetaGamma[allGammas,allGammas] %*%
             t(jacobian);
        correctedCovariancePrevalence <- jacobian %*%
             correctedCovarianceThetaGamma[allGammas,allGammas] %*%
             t(jacobian);
        sandwichCovariancePrevalence <- jacobian %*%
             sandwichCovarianceThetaGamma[allGammas,allGammas] %*%
             t(jacobian);
      } else {
        uncorrectedCovariancePrevalence <- NULL;
        correctedCovariancePrevalence <- NULL;
        sandwichCovariancePrevalence <- NULL;
      }
      return(list( uncorrectedCovarianceThetaGamma=uncorrectedCovarianceThetaGamma,
                    # Similar to the covariance matrix, but without
                    # correcting for missing class labels as in Turner
                   correctedCovarianceThetaGamma=correctedCovarianceThetaGamma,
                    # The estimated covariance matrix for all of
                    # the model parameters, adjusting for the unknown
                    # class labels
                   sandwichCovarianceThetaGamma=sandwichCovarianceThetaGamma,
                    # The sandwich covariance matrix for the model
                    # parameters, which may be more robust to
                    # misspecification of the within-subject
                    # correlation (see Liang and Zeger 1986)
                   uncorrectedCovariancePrevalence=uncorrectedCovariancePrevalence,
                    # Estimators for the covariance of class prevalences (overall
                    # proportions) based on the respective estimators for the
                    # covariance of the model parameters.
                   correctedCovariancePrevalence=correctedCovariancePrevalence,
                   sandwichCovariancePrevalence=sandwichCovariancePrevalence,
                   thetaIndex=thetaIndex,
                    # Tells which rows and columns of the covariance
                    # matrix refer to thetas (linear regression
                    # parameters)
                   gammaIndex=gammaIndex,
                    # Tells which rows and columns of the covariance
                    # matrix refer to gammas (logistic regression
                    # parameters)
                   I1=I1,
                   I2=I2,
                   I3=I3)); # I1, I2, and I3 are described in our paper
                        # (Dziak et al) and are based on combining
                        # the approach of Turner (2000) with a
                        # sandwich estimation approach;
    }
  #####################################################################
  ## Main body of MixTVEM function
  ## Process the subject ID's;
     if (!is.integer(id)) {
       temporary.id <- as.integer(as.factor(id));
     } else {
       temporary.id <- id;
     }
     if (is.na(min.time)) {min.time <- min(time);}
     if (is.na(max.time)) {max.time <- max(time);}
     numTotal <- length(id);
     intId <- rep(0,numTotal);
     stopifnot(is.integer(unique(temporary.id)));
     numSubjects <- length(unique(temporary.id));
     for (i in 1:numSubjects) {
      intId[which(temporary.id==unique(temporary.id)[i])] <- i;
     }
     stopifnot(length(unique(intId))==numSubjects);
     stopifnot(all.equal(unique(intId),1:numSubjects));
   ## Decide on a reference class;
     if (is.na(referenceClass)) {referenceClass <- 1;}
     stopifnot(referenceClass>0);
     stopifnot(referenceClass<=numClasses);
   ## Process the class membership predictor variables S;
     if (!is.null(scov)) {
      scov <- as.matrix(scov);
      stopifnot(nrow(scov)==numTotal);
      S <- matrix(0,numSubjects,1+ncol(scov));
      for (i in 1:numSubjects) {
       theseObs <- which(intId==i);
       S[i,] <- c(1, scov[min(theseObs),]);
      }
     } else {
      S <- matrix(1,numSubjects,1);
     }
     if (is.null(colnames(scov))) {
       colnames(S) <- paste("S",0:(ncol(S)-1),sep="");
     };
     ## Process the time variable;
     stopifnot(!is.null(time));
     stopifnot((deg==1)|(deg==2)|(deg==3));
     time <- as.matrix(time);
     stopifnot(nrow(time)==numTotal);
     stopifnot(ncol(time)==1);
     stopifnot(is.numeric(time));
     if (is.null(colnames(time))) {
      colnames(time) <- "Time";
     }
     timeBasis <- list();
     designMatrix <- NULL;
     whichBeta <-  NULL;
   ## Construct the first part of the design matrix including ... ;
     ## ... the intercept column if there are no time-varying covariates;
       if (is.null(tcov)) {    interceptColumn <- matrix(1,numTotal,1);
        colnames(interceptColumn) <- "Intercept";
        designMatrix <- cbind(designMatrix,interceptColumn);
        whichBeta <- c(whichBeta,0);
       }
     ## ... and the covariates without time-varying effects;
       if (!is.null(xcov)) {
        xcov <- as.matrix(xcov);
        if (min(apply(xcov,2,var))<1e-10) {
          stop("Please do not include a constant column in xcov.");
        }
        stopifnot(nrow(xcov)==numTotal);
        numXCov <- ncol(xcov);
        if (is.null(colnames(xcov))) {
         colnames(xcov) <- paste("X",1:ncol(xcov),sep="");
        };
        designMatrix <- cbind(designMatrix,xcov);
       } else {
        numXCov <- 0;
       }
       whichBeta <- c(whichBeta,rep(0,numXCov));
     ## Get the tcov matrix ready;
       if (is.null(tcov)) {
        numTCov <- 0;
        stop(paste("tcov is null.",
              "No time-varying betas or effects are in the model."));
       } else {
        tcov <- as.matrix(tcov);
        stopifnot(nrow(tcov)==numTotal);
        numTCov <- ncol(tcov);
        if (is.null(colnames(tcov))) {
         colnames(tcov) <- paste("TV",1:ncol(tcov),sep="");
        };
   ## Create the scale vector to be used with the penalty for the time-varying
   ## covariates;
    if (!identical(as.vector(tcov[,1]),rep(1,numTotal))) {
      warning("tcov does not seem to contain an intercept (trajectory) column.");
    }
    tcov.scale <- apply(tcov,2,sd);
    if (length(tcov.scale)>1) {
      if (min(tcov.scale[-1])<1e-10) {
        stop(paste("Please include the intercept column as the first",
             "column in tcov, and do not include any other columns",
             "which are constant across all",
             "subjects and assessments."));
      }
    }
    if (identical(as.vector(tcov[,1]),rep(1,numTotal))) {
      tcov.scale[1] <- 1; # do not scale intercept column;
    }
   ## Create the basis functions;
    if (length(numInteriorKnots)!=1) {stop();}
    stopifnot(numInteriorKnots>0);
    num.intervals <- numInteriorKnots+1;
    dx <- (max.time-min.time)/num.intervals;
    all.knot.locations <- seq(min.time-(deg)*dx,
                max.time+(deg)*dx,
                by=dx);
    all.knot.locations[1] <- all.knot.locations[2]-1e-8;
    all.knot.locations[length(all.knot.locations)] <-
            all.knot.locations[length(all.knot.locations)-1]+1e-8;
    interior.knot.locations <- seq(min.time+dx,
                 max.time-dx,
                 by=dx);
    timeGrid <- seq(min.time,max.time,length=gridSize);
    timeBasis <- spline.des(all.knot.locations,
                time,
                deg+1,
                rep(0,length(time)),
                outer.ok=TRUE)$design;
             # Adapted from Eilers and Marx, 1996;
    colnames(timeBasis) <- paste(colnames(time),".Spline.",
                       1:ncol(timeBasis),
                       sep="");
    timeBasisByGrid <- spline.des(all.knot.locations,
                timeGrid,
                deg+1,
                rep(0,length(timeGrid)),
                outer.ok=TRUE)$design;
             # Adapted from Eilers and Marx, 1996;
    colnames(timeBasisByGrid) <- paste(colnames(time),".Spline.",
                       1:ncol(timeBasisByGrid),
                       sep="");
    # Now generate regression matrix;
    for (j in 1:numTCov) {
      covariateTimesTimeBasis <- tcov[,j,drop=TRUE]*timeBasis;
                    # Elementwise product;
      colnames(covariateTimesTimeBasis) <- paste(colnames(tcov)[j],
                           "times",
                           colnames(timeBasis),
                           sep=".");
      designMatrix <- cbind(designMatrix, covariateTimesTimeBasis);
      whichBeta <- c(whichBeta,rep(j,ncol(covariateTimesTimeBasis)));
    }
    stopifnot(sum(is.nan(designMatrix))==0);
    stopifnot(sum(is.null(designMatrix))==0);
    if(sum(is.na(designMatrix))>0) {
      stop("Missing data is not yet supported in this software.");
    }
   }
   ## Create the penalty weight matrix (see Eilers and Marx, 1996)
   diffMatrix <- crossprod(diff(diff(diag(rep(1,ncol(timeBasis))))));
   penaltyMatrix <- matrix(0,ncol(designMatrix),ncol(designMatrix));
   for (j in 1:numTCov) {
     indices <- numXCov+(j-1)*ncol(timeBasis)+(1:ncol(timeBasis));
     stopifnot(length(indices)==nrow(diffMatrix));
     stopifnot(length(indices)==ncol(diffMatrix));
     penaltyMatrix[indices,indices] <- diffMatrix/(tcov.scale[j]^2);
   }
   #######################################################################
   ## Run a loop to find the best seed;
   logLikBySeed <- rep(NA,numStarts);
   rhoBySeed <- rep(NA,numStarts);
   proportionNuggetBySeed <- rep(NA,numStarts);
   weightedRSSBySeed <- rep(NA,numStarts);
   if (numStarts > 1) {
     if (!is.na(seed)) {
     };
     seeds <- round(runif(numStarts)*1e8);
   } else {
     if (!is.na(seed)) {
       seeds <- seed;
     } else {
       seeds <- round(runif(1)*1e8);
     }
   }
   postProbsList <- list();
   for (i in 1:numStarts) {
     set.seed(seeds[i]);
     print(paste("Trying seed=",seeds[i]));
     thisFit <- MixTVEMFitInner(convergenceCriterion=convergenceCriterion,
                intId=intId,
                maxIterations=maxIterations/2,
                maxVarianceRatio=maxVarianceRatio,
                numClasses=numClasses,
                penaltyMatrix=penaltyMatrix,
                referenceClass=referenceClass,
                roughnessPenalty=(1e9)*sd(dep),
                rho=0,
                S=S,
                time=time,
                X=designMatrix,
                Y=dep);
    if (assumeIndependence==TRUE) {
      estimates <- list(rho=0,proportionNugget=1);
    } else {
      estimates <- estimateRho(thisFit, time);
    }
    estimatedProportionNugget <- estimates$proportionNugget;
    estimatedRho <- estimates$rho;
    thisFit <- MixTVEMFitInner(convergenceCriterion=convergenceCriterion,
                intId=intId,
                maxIterations=maxIterations/2,
                maxVarianceRatio=maxVarianceRatio,
                numClasses=numClasses,
                penaltyMatrix=penaltyMatrix,
                proportionNugget=estimatedProportionNugget,
                referenceClass=referenceClass,
                roughnessPenalty=(1e9)*sd(dep),
                rho=estimatedRho,
                S=S,
                time=time,
                X=designMatrix,
                Y=dep);
    logLikBySeed[i] <- thisFit$logLik;
    rhoBySeed[i] <- estimatedRho;
    weightedRSSBySeed[i] <- thisFit$weightedRSS;
    postProbsList[[i]] <- thisFit$postProbsBySub;
    proportionNuggetBySeed[[i]] <- estimatedProportionNugget;
   }
   bestSeed <- seeds[which.max(logLikBySeed)];
   bestRho <- rhoBySeed[which.max(logLikBySeed)];
   bestProportionNugget <- proportionNuggetBySeed[which.max(logLikBySeed)];
   #######################################################################
   ## Find the best tuning parameter;
     f <- function(lambda) {
       set.seed(bestSeed);
       print(paste("Trying lambda=",round(lambda,5)));
       thisFit <- MixTVEMFitInner(convergenceCriterion=convergenceCriterion,
                    intId=intId,
                    maxIterations=maxIterations,
                    maxVarianceRatio=maxVarianceRatio,
                    numClasses=numClasses,
                    penaltyMatrix=penaltyMatrix,
                    proportionNugget=bestProportionNugget,
                    referenceClass=referenceClass,
                    roughnessPenalty=lambda,
                    rho=bestRho,
                    S=S,
                    time=time,
                    X=designMatrix,
                    Y=dep);
       if (useAIC==TRUE) { # Rather sloppy call to a global parameter;
         return(thisFit$aic);
       } else {
         return(thisFit$bic);
       }
     }
     # Find the right order of magnitude;
     lambdasSearch1 <- 10^(seq(-3,floor(log10(numTotal))))*sd(dep);
     resultsSearch1 <- sapply(lambdasSearch1, f);
     bestLambda1 <- lambdasSearch1[which.min(resultsSearch1)];
     print(cbind(lambdasSearch1,resultsSearch1));
     # Find the right approximate value;
     lambdasSearch2 <- (10^seq(-0.6,0.6,length=5))*bestLambda1;
     resultsSearch2 <- sapply(lambdasSearch2, f);
     bestLambda2 <- lambdasSearch2[which.min(resultsSearch2)];
     print(cbind(lambdasSearch1,resultsSearch1));
     # Zoom in closer;
     lambdasSearch3 <- (10^seq(-0.2,0.2,length=5))*bestLambda2;
     resultsSearch3 <- sapply(lambdasSearch3, f);
     bestLambda3 <- lambdasSearch3[which.min(resultsSearch3)];
     print(cbind(lambdasSearch1,resultsSearch1));
     # Good enough;
     lambda <- bestLambda3;
## Finally do the analysis;
     set.seed(bestSeed);
     bestFit <- MixTVEMFitInner(convergenceCriterion=convergenceCriterion,
                  intId=intId,
                  maxIterations=maxIterations,
                  maxVarianceRatio=maxVarianceRatio,
                  numClasses=numClasses,
                  penaltyMatrix=penaltyMatrix,
                  proportionNugget=bestProportionNugget,
                  referenceClass=referenceClass,
                  rho=bestRho,
                  roughnessPenalty=lambda,
                  S=S,
                  time=time,
                  X=designMatrix,
                  Y=dep);
   ## Now record the fitted values on the original assessment times;
     fitted <- designMatrix%*%bestFit$theta;
     fittedCoefficients <- list();
     for (j in 1:numTCov) {
       fittedCoefficients[[j]] <- timeBasis%*%
                    bestFit$theta[which(whichBeta==j),];
     }
     estimate <- list();
     for (which.coef in 1:numTCov) {
       estimate[[which.coef]] <- list();
       for (which.class in 1:numClasses) {
         estimate[[which.coef]][[which.class]] <-
           fittedCoefficients[[which.coef]][order(time),which.class];
       }
    }
   ## Record the fitted values on the fit grid;
     fittedCoefficientsByGrid <- list();
     for (j in 1:numTCov) {
       fittedCoefficientsByGrid[[j]] <- timeBasisByGrid%*%
                    bestFit$theta[which(whichBeta==j),];
     }
     estimate <- list();
     for (which.coef in 1:numTCov) {
       estimate[[which.coef]] <- list();
       for (which.class in 1:numClasses) {
         estimate[[which.coef]][[which.class]] <-
           fittedCoefficientsByGrid[[which.coef]][order(timeGrid),which.class];
       }
    }
   ## Get the standard errors;
    if (getSEs) {
         covarMats <- MixTVEMCovMats(fittedProb=bestFit$fittedProb,
            gamma=bestFit$gamma,
            intId=intId,
            mu=bestFit$fittedY,
            numSubjects=numSubjects,
            numTotal=numTotal,
            penalty=lambda*penaltyMatrix,
            postProb=bestFit$postProbsBySub,
            proportionNugget=bestFit$proportionNugget,
            referenceClass=referenceClass,
            rho=bestFit$rho,
            S=S,
            sigsq.total=bestFit$sigsq.total,
            theta=bestFit$theta,
            X=designMatrix,
            Y=dep);
        if (sum(is.na(covarMats$sandwichCovarianceThetaGamma))>0) {
           warning("Could not compute sandwich standard errors.");
        }
        fittedCoefficientsStdErrUncorrected <- list();
        fittedCoefficientsStdErrCorrected <- list();
        fittedCoefficientsStdErrSandwich <- list();
        fittedCoefficientsStdErrUncorrectedByGrid <- list();
        fittedCoefficientsStdErrCorrectedByGrid <- list();
        fittedCoefficientsStdErrSandwichByGrid <- list();
        for (j in 1:numTCov) {
          fittedCoefficientsStdErrUncorrected[[j]] <-
                     matrix(NA,numTotal,numClasses);
          fittedCoefficientsStdErrCorrected[[j]] <-
                     matrix(NA,numTotal,numClasses);
          fittedCoefficientsStdErrSandwich[[j]] <-
                     matrix(NA,numTotal,numClasses);
          fittedCoefficientsStdErrUncorrectedByGrid[[j]] <-
                     matrix(NA,gridSize,numClasses);
          fittedCoefficientsStdErrCorrectedByGrid[[j]] <-
                     matrix(NA, gridSize,numClasses);
          fittedCoefficientsStdErrSandwichByGrid[[j]] <-
                     matrix(NA, gridSize,numClasses);
          for (class in 1:numClasses) {
            thisCovmatUncorrected <-
               covarMats$uncorrectedCovarianceThetaGamma[
                   covarMats$thetaIndex[which(whichBeta==j),class],
                   covarMats$thetaIndex[which(whichBeta==j),class]];
            thisCovmatCorrected <-
               covarMats$correctedCovarianceThetaGamma[
                   covarMats$thetaIndex[which(whichBeta==j),class],
                   covarMats$thetaIndex[which(whichBeta==j),class]];
            thisCovmatSandwich <-
               covarMats$sandwichCovarianceThetaGamma[
                   covarMats$thetaIndex[which(whichBeta==j),class],
                   covarMats$thetaIndex[which(whichBeta==j),class]];
            fittedCoefficientsStdErrUncorrected[[j]][,class] <- sqrt(diag(
                              as.matrix(timeBasis %*%
                              thisCovmatUncorrected %*%
                              t(timeBasis))));
            fittedCoefficientsStdErrUncorrectedByGrid[[j]][,class] <- sqrt(diag(
                              as.matrix(timeBasisByGrid %*%
                              thisCovmatUncorrected %*%
                              t(timeBasisByGrid))));
            fittedCoefficientsStdErrCorrected[[j]][,class] <- sqrt(diag(
                              as.matrix(timeBasis %*%
                              thisCovmatCorrected %*%
                              t(timeBasis))));
            fittedCoefficientsStdErrCorrectedByGrid[[j]][,class] <- sqrt(diag(
                              as.matrix(timeBasisByGrid %*%
                              thisCovmatCorrected %*%
                              t(timeBasisByGrid))));
            fittedCoefficientsStdErrSandwich[[j]][,class] <- sqrt(diag(
                              as.matrix(timeBasis %*%
                              thisCovmatSandwich %*%
                              t(timeBasis))));
            fittedCoefficientsStdErrSandwichByGrid[[j]][,class] <- sqrt(diag(
                              as.matrix(timeBasisByGrid %*%
                              thisCovmatSandwich %*%
                              t(timeBasisByGrid))));
          }
        }
     } else {
       # Standard errors were not requested;
       covarMats <- NULL;
       fittedCoefficientsStdErrUncorrected <- NULL;
       fittedCoefficientsStdErrUncorrectedByGrid <- NULL;
       fittedCoefficientsStdErrCorrected <- NULL;
       fittedCoefficientsStdErrCorrectedByGrid <- NULL;
       fittedCoefficientsStdErrSandwich <- NULL;
       fittedCoefficientsStdErrSandwichByGrid <- NULL;
       fittedCoefficientsSEByGrid <- NULL;
     }
     if (doPlot==TRUE&length(time)>10) {
         stopifnot(numClasses<10);
         stopifnot(numTCov<4);
         theColors <- c("red","blue","darkgreen","purple","pink",
                  "cyan","magenta","green","darkgray");
         if (numTCov==2) {par(mfrow=c(2,1));}
         if (numTCov>2) {par(mfrow=c(2,2));}
         for (which.coef in 1:numTCov) {
           for (which.class in 1:numClasses) {
           if (which.class==1) {
             plot(x=timeGrid[order(timeGrid)],
                y=fittedCoefficientsByGrid[[which.coef]][order(timeGrid),which.class],
                ylim=c(min(unlist(fittedCoefficientsByGrid[[which.coef]])),
                   max(unlist(fittedCoefficientsByGrid[[which.coef]]))),
                col=theColors[which.class],type="l",
                xlab="Time",
                ylab=paste("Coefficient",which.coef));
           } else {
             lines(x=timeGrid[order(timeGrid)],
                y=fittedCoefficientsByGrid[[which.coef]][order(timeGrid),which.class],
                col=theColors[which.class]);
           }
           markers <- round(quantile(1:length(timeGrid),(1:9)/10));
           text(x=timeGrid[order(timeGrid)][markers],
             y=fittedCoefficientsByGrid[[which.coef]][
               order(timeGrid),which.class][markers],
             col=theColors[which.class],
             labels=which.class);
           if (getSEs) {
             if (sum(is.na(fittedCoefficientsStdErrSandwichByGrid[[which.coef]]))==0) {
               lines(x=timeGrid[order(timeGrid)],
                  y=fittedCoefficientsByGrid[[which.coef]][order(timeGrid),which.class]+1.96*
                    #fittedCoefficientsStdErrUncorrectedByGrid[[which.coef]][order(timeGrid),which.class],
                    fittedCoefficientsStdErrSandwichByGrid[[which.coef]][order(timeGrid),which.class],
                  col=theColors[which.class],
                  lty="dotted");
               lines(x=timeGrid[order(timeGrid)],
                  y=fittedCoefficientsByGrid[[which.coef]][order(timeGrid),which.class]-1.96*
                    #fittedCoefficientsStdErrUncorrectedByGrid[[which.coef]][order(timeGrid),which.class],
                    fittedCoefficientsStdErrSandwichByGrid[[which.coef]][order(timeGrid),which.class],
                  col=theColors[which.class],
                  lty="dotted");
             }
           }
         }
       }
     }
     prop.best.logLik <- mean(logLikBySeed>max(logLikBySeed)-.1);
     prop.best.weightedRSS <- mean(weightedRSSBySeed<min(weightedRSSBySeed)+.1);
     best.agree <- (weightedRSSBySeed[which.max(logLikBySeed)] -
                       min(weightedRSSBySeed) < .1 );
     cat("MixTVEM R Function \n");
     cat(sprintf("Number of subjects: %29.0f \n",numSubjects));
     cat(sprintf("Total number of observations: %19.0f \n",numTotal));
     if(deg==1) {cat("Effect of time between knots treated as linear \n")};
     if(deg==2) {cat("Effect of time between knots treated as quadratic \n")};
     if(deg==3) {cat("Effect of time between knots treated as cubic \n")};
     if(min(apply(bestFit$fittedProb,2,mean))<.025) {
       warning("The smallest fitted class is very small");
     }
     if (!bestFit$converged) {
       warning("EM algorithm did not converge");
     }
     cat(sprintf("Roughness penalty weight: %23.2f \n", lambda));
     cat(sprintf("Weighted RSS statistic: %25.2f \n", bestFit$weightedRSS));
     cat(sprintf("Weighted GCV statistic: %25.2f \n", bestFit$weightedGCV));
     cat(sprintf("Log-likelihood: %33.2f \n",bestFit$logLik));
     cat(sprintf("AIC: %44.2f \n",bestFit$aic));
     cat(sprintf("BIC: %44.2f \n",bestFit$bic));
     if (numStarts>1) {
       cat("Proportion of starting values giving approximately the best obtained ...\n");
       cat(sprintf(" ... log-likelihood: %28.2f \n",prop.best.logLik));
       cat(sprintf(" ... weighted sum squared error: %20.2f \n",prop.best.weightedRSS));
       if (!best.agree) {
         print("These criteria for the best starting value may not agree.");
         print("The starting value that gave the best log-likelihood was used.");
       }
     }
     cat(sprintf("Estimated autocorrelation parameter rho: %8.3f \n", bestFit$rho));
     cat(sprintf("Estimated proportion nugget: %20.3f \n", bestFit$proportionNugget));
     cat(sprintf("Count number of parameters: %21.2f \n", bestFit$np));
     cat(sprintf("Smoothed number of parameters: %18.2f \n", bestFit$enp));
     cat("Class proportions: \n");
     cat(round(apply(bestFit$fittedProb,2,mean),4)); cat("\n");
     cat("Total standard deviations: \n");
     cat(round(sqrt(bestFit$sigsq.total),4)); cat("\n");
   # Provide the logistic regression output;
    logisticRegOutput <- NULL;
    this.nonref.class.index <- 0;
    for (which.class in 1:numClasses) {
        if (which.class==referenceClass) {next;}
        this.nonref.class.index <- this.nonref.class.index +1;
        this.class.est <- as.vector(bestFit$gamma[,which.class]);
        if (getSEs) {
            this.class.se.uncorrected <- as.vector(sqrt(diag(as.matrix(
                             covarMats$uncorrectedCovarianceThetaGamma
                             [covarMats$gammaIndex[,this.nonref.class.index,drop=FALSE],
                              covarMats$gammaIndex[,this.nonref.class.index,drop=FALSE]]))));
            this.class.se.corrected <- as.vector(sqrt(diag(as.matrix(
                             covarMats$correctedCovarianceThetaGamma
                             [covarMats$gammaIndex[,this.nonref.class.index,drop=FALSE],
                              covarMats$gammaIndex[,this.nonref.class.index,drop=FALSE]]))));
            this.class.se.sandwich <- as.vector(sqrt(diag(as.matrix(
                             covarMats$sandwichCovarianceThetaGamma
                             [covarMats$gammaIndex[,this.nonref.class.index,drop=FALSE],
                              covarMats$gammaIndex[,this.nonref.class.index,drop=FALSE]]))));
            this.class.z.sandwich <- this.class.est/this.class.se.sandwich;
            this.class.p.sandwich <- 2*(1-pnorm(abs(this.class.z.sandwich)));
        } else {
            this.class.se.uncorrected <- NA;
            this.class.se.corrected <- NA;
            this.class.se.sandwich <- NA;
            this.class.z.sandwich <- NA;
            this.class.p.sandwich <- NA;
        }
        this.class.output <- data.frame(Column=paste("S",1:length(this.class.est),
                                                      sep=""),
                                        Class=which.class,
                                        Estimate=this.class.est,
                                        SEUncorrected=this.class.se.uncorrected,
                                        SECorrected=this.class.se.corrected,
                                        SESandwich=this.class.se.sandwich,
                                        z=round(this.class.z.sandwich,8),
                                        p=round(this.class.p.sandwich,8));
        logisticRegOutput <- rbind(logisticRegOutput,this.class.output);
    }
    cat("Logistic regression for class membership: \n");
    print(logisticRegOutput);
    nontvem.reg.output <- NULL;
    if (!is.null(xcov)) {
        for (which.class in 1:numClasses) {
            if (which.class==referenceClass) {next;}
            this.class.est <- bestFit$theta[which(whichBeta==0),which.class];
            these.indices <- covarMats$thetaIndex[which(whichBeta==0),which.class];
            if (getSEs) {
                this.class.se.uncorrected <- as.vector(sqrt(diag(covarMats$uncorrectedCovarianceThetaGamma[
                                          these.indices, these.indices,drop=FALSE])));
                this.class.se.corrected <- as.vector(sqrt(diag(covarMats$correctedCovarianceThetaGamma[
                                          these.indices, these.indices,drop=FALSE])));
                this.class.se.sandwich <- as.vector(sqrt(diag(covarMats$sandwichCovarianceThetaGamma[
                                          these.indices, these.indices,drop=FALSE])));
                this.class.z.sandwich <- this.class.est/this.class.se.sandwich;
                this.class.p.sandwich <- 2*(1-pnorm(abs(this.class.z.sandwich)));
            } else {
                this.class.se.uncorrected <- NA;
                this.class.se.corrected <- NA;
                this.class.se.sandwich <- NA;
                this.class.z.sandwich <- NA;
                this.class.p.sandwich <- NA;
            }
            this.class.output <- data.frame(Column=paste("X",
                                                   1:length(this.class.est),sep=""),
                                            Class=which.class,
                                            Estimate=this.class.est,
                                            SEUncorrected=this.class.se.uncorrected,
                                            SECorrected=this.class.se.corrected,
                                            SESandwich=this.class.se.sandwich,
                                            z.sandwich=round(this.class.z.sandwich,8),
                                            p.sandwich=round(this.class.p.sandwich,8));
            nontvem.reg.output <- rbind(nontvem.reg.output,this.class.output);
        }
        cat("Non-time-varying coefficients: \n");
        print(nontvem.reg.output);
    }
   # Return the answers;
       return(list( bestFit=bestFit,
                           # Answer from the MixTVEMFitInner function,
                           # as obtained for the best starting value.
                    allBSplineKnots=all.knot.locations,
                           # List of all knot locations for the spline
                    bestSeed=bestSeed,
                           # Selected random number seed for the best
                           # solution obtained.
                    covarMats=covarMats,
                           # Answer from MixTVEMCovMats for the
                           # estimates obtained by the best starting values.
                    dep=dep,
                           # The dependent variable as a vector; if there
                           # are no NA's (missing data) in the data provided
                           # in the input, then this is the same as the dep
                           # vector that was provided.  It is provided again
                           # here in the output list for convenience in
                           # comparing observed to fitted values.
                           # Also included as a member of bestFit.
                    beta=fittedCoefficients,
                           # The fitted values for the coefficient functions
                           # at each assessment time value
                    betaByGrid=fittedCoefficientsByGrid,
                           # The fitted values for the coefficient functions
                           # at time value on a regular grid of GridSize points
                    betaSEUncorrected=fittedCoefficientsStdErrUncorrected,
                    betaSECorrected=fittedCoefficientsStdErrCorrected,
                    betaSESandwich=fittedCoefficientsStdErrSandwich,
                           # The standard errors for the coefficient functions
                           # at each assessment time value
                    betaSEUncorrectedByGrid=fittedCoefficientsStdErrUncorrectedByGrid,
                    betaSECorrectedByGrid=fittedCoefficientsStdErrCorrectedByGrid,
                    betaSESandwichByGrid=fittedCoefficientsStdErrSandwichByGrid,
                           # The standard errors for the coefficient functions
                           # at time value on a regular grid of GridSize points
                    fittedValues=fitted,
                           # The fitted values for the dependent variable
                           # for each class, at each assessment time value.
                           # Also included as a member of bestFit.
                    id=id,
                    intId=intId,
                           # The identification variable provided and the
                           # possibly recoded interior version.
                    knotLocations=interior.knot.locations,
                           # Interior knots of the spline.  This may differ
                           # from allBSplineKnots because a B-spline may have
                           # invisible "exterior" knots which help to make the
                           # math work.
                    logisticRegOutput=logisticRegOutput,
                           # Information on the logistic regression for
                           # predicting class memberships.
                    logLikBySeed=logLikBySeed,
                           # Fitted log-likelihood for each random seed which was
                           # tried.
                    penaltyMatrix=penaltyMatrix,
                    referenceClass=referenceClass,
                           # The reference class for the logistic regression for
                           # predicting class memberships.
                    rhoBySeed=rhoBySeed,
                           # Estimated correlation parameter for each random
                           # seed which was tried.
                    S=S,
                           # The matrix of subject-level covariates.  For
                           # convenience, they are presented in a one-row-per
                           # subject format without duplication; an intercept
                           # column is also included.  Multiplying these
                           # by the fitted gammas from bestFit obtains the
                           # generalized linear model linear predictors
                           # for class membership.
                    time=as.vector(time),
                           # The vector of assessment times.  It is provided again
                           # here in the output list for convenience.
                    timeBasis=timeBasis,
                           # The basis matrix used to express the effect
                           # of time.
                    timeGrid=timeGrid,
                           # The time values used to arrange predictions
                           # on a regular set of points for graphing.
                    weightedRSSBySeed=weightedRSSBySeed,
                           # Fitted weighted RSS for each random seed
                           # tried.
                    whichBeta=whichBeta,
                           # Used for interpreting the results of bestFit and
                           # covarMats.  It tells which time-varying coefficient
                           # is represented by each column of the design matrix.
                    X=designMatrix
                           # The design matrix used in the regression, including
                           # all of the columns for all of the splines.
                    ));
}

##################################################################
