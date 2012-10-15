spsm <- function(pCADFtest.results, alpha = 0.05)
{
  ## Sequential Panel Selection Method
  ##
  ## pCADFtest.results:	pCADFtest object from pCADFtest()
  ## alpha:      	significance level

  ## Initialize some values
  N <- nrow(pCADFtest.results$individual.tests) # N: number of time series
  K.rej <- NULL                          # K.rej: variables identified as I(0)

  ## Define the full set of time series
  S <- 1:N

  ## continue only if the panel test rejects
  if(pCADFtest.results$p.value < alpha)
  {
    ## identify the minimum p-value of univariate tests (u.rej)
    pvalues <- pCADFtest.results$individual.tests[,1]
    r <- which.min(pvalues)
    K.rej <- r
    pvalues[r] <- 9999
    ## S1: all the series except the one under test
    S1 <- S[-r]
    rej <- TRUE
    while((length(S1) > 0)&(rej == TRUE))
    {
      sub.p.values <- pvalues[S1]
      if (pCADFtest.results$corr == FALSE)
      {
	# Perform Choi's test (inverse normal combination)
	Z    <- sum(qnorm(sub.p.values))/sqrt(length(sub.p.values))   # simple combination statistic
	pval <- pnorm(Z)                      # pvalue of Choi's test
      }
      else
      {
	if (length(S1) > 1)
	{
	  # Perform Hartung's adjustment
	  hart <- Hartung(sub.p.values)       # Hartung's correction
	  Z    <- hart$statistic
	  pval <- hart$p.value
	}
	else
	{
	  pval <- sub.p.values
	}
      }
      rej <- (pval < alpha)
      if (rej == TRUE)
      {
	r <- c(r, which.min(pvalues))
	K.rej <- r
	pvalues[r] <- 9999
	S1 <- S[-r]
      }
    }
  }
  return(K.rej)
}
