pCADFtest <- function(Y, X=NULL, covariates=NULL, crosscorr=0.10, 
	     type="trend", data=list(), max.lag.y=1, 
	     min.lag.X=0, max.lag.X=0, dname=NULL, 
	     criterion=c("none", "BIC", "AIC", "HQC", "MAIC"), ...)
{
  T <- dim(Y)[1]
  N <- dim(Y)[2]
  if (is.ts(Y)==FALSE) Y <- ts(Y)
  if (is.null(X)==FALSE) {if (is.ts(X)==FALSE) X <- ts(X)}
  fake.Y <- ts(Y, start=1, frequency=1)

  if (is.null(covariates)) method <- "Panel-ADF test" else method <- "Panel-CADF test"

  # the covariate is the first difference of the first principal component of Y
  # the same covariate is used for all individual tests
  if (is.null(covariates)==FALSE)
  {
    if (covariates[1]=="PC")
    {
      YY <- na.trim(Y)
      X <- ts(princomp(YY, cor=TRUE)$scores[,1], start=start(YY), frequency=frequency(YY))
      XX <- X
      for (i in 2:N) XX <- cbind(XX, X)
      X <- diff(XX)
      X <- window(X, start=start(Y), end=end(Y), frequency=frequency(Y), extend=TRUE)
      covariates <- 1:N
    }
    else
    # for the i-th test, the covariate is the difference of the average of the other Y's
    if (covariates[1]=="DY")
    {
      X <- matrix(NA, T, N)
      for (i in 1:N)
      X[,i] <- apply(Y[,-i], 1, "mean")
      X <- ts(X, start=start(Y), frequency=frequency(Y))
      X <- diff(X)
      X <- window(X, start=start(Y), end=end(Y), frequency=frequency(Y), extend=TRUE)
      covariates <- 1:N
    }
  }

  if (length(type)==1)
    type <- rep(type, N)

  if (length(max.lag.y)==1)
    max.lag.y <- rep(max.lag.y, N)

  if (length(min.lag.X)==1)
    min.lag.X <- rep(min.lag.X, N)

  if (length(max.lag.X)==1)
    max.lag.X <- rep(max.lag.X, N)

  criterion <- match.arg(criterion)

  # individual.results: Nx5 matrix to store 
  # (1) individual p-values
  # (2) individual rho2
  # (3-5) p, q1, q2: orders of the model CADF(p,q1,q2)
  individual.results <- matrix(NA, N, 5)
  colnames(individual.results) <- c("p.value", "rho2", "p", "q1", "q2")
  rownames(individual.results) <- colnames(Y)

  # resids: TxN matrix of residuals from individual tests
  resids <- matrix(NA, T, N)
  resids <- ts(resids, start=start(Y), end=end(Y), frequency=frequency(Y))

  # Perform individual tests
  for (i in 1:N)
  {
    # dep: dependent variable
    depv <- Y[,i]
    if (is.null(covariates))
    {
      test <- CADFtest(depv, max.lag.y=max.lag.y[i], 
	      min.lag.X=min.lag.X[i], max.lag.X=max.lag.X[i], 
	      type=type[i], criterion=criterion, ...)
      r <- residuals(test)
      r <- window(r, start=start(Y), end=end(Y), frequency=frequency(Y), extend=TRUE)
      test$parameter <- NA
    }
    else
    {
      # covariates
      covs <- X[,which(covariates==i)]
      covs <- window(covs, start=start(depv), end=end(depv), frequency=frequency(depv), extend=TRUE)
      test <- CADFtest(depv~covs, max.lag.y=max.lag.y[i], 
	      min.lag.X=min.lag.X[i], max.lag.X=max.lag.X[i], 
	      type=type[i], criterion=criterion, ...)
      r <- residuals(test)
      r <- window(r, start=start(fake.Y), end=end(fake.Y), frequency=frequency(fake.Y), extend=TRUE)
      r <- ts(r, start=start(Y), end=end(Y), frequency=frequency(Y))
    }
    resids[,i] <- r
    tpv <- test$p.value
    tpv <- max(tpv, 1e-16); tpv <- min(tpv, 1 - 1e-16)
    individual.results[i,1] <- tpv              # p-value 
    individual.results[i,2] <- test$parameter   # estimated rho^2 (for CADF only)
    individual.results[i,3] <- test$max.lag.y   # selected max lag of the dependent 
    individual.results[i,4] <- test$max.lag.X   # selected max lag  of the covariate (for CADF only)
    individual.results[i,5] <- test$min.lag.X   # selected max lead of the covariate (for CADF only)
  }
  # Combination of individual tests
  ptest <- Pesaran(resids)
  if (ptest$p.value >= crosscorr)
  {
    # Perform Choi's test (inverse normal combination)
    Z    <- sum(qnorm(individual.results[,1]))/sqrt(N)   # simple combination statistic
    pval <- pnorm(Z)                                     # pvalue of Choi's test
  }
  else
  {
    # Perform Hartung's adjustment
    hart <- Hartung(individual.results[,1])              # Hartung's correction
    Z    <- hart$statistic
    pval <- hart$p.value
  }
  
   panel.results <- list(statistic=c("test statistic"=Z),
 			parameter=c("mean.rho2" = mean(individual.results[,2])),
 			method=method,
 			p.value=as.vector(pval),
			corr=(ptest$p.value < crosscorr),
			individual.tests=individual.results,
			Pesaran=ptest)
   class(panel.results) <- c("pCADFtest", "htest")
   return(panel.results)
}
