Simes <- function(pCADFtest.results, alpha=0.05)
{
  # This function computes the panel unit root Simes test.
  # Arguments:
  # pCADFtest.results: an object of class pCADFtest
  # alpha            : the level of the test (can be a vector)
  # Values:
  # outcome          : logical. TRUE = "Don't reject", FALSE = "Reject" the null
  #		       it is a vector if alpha is a vector. 
  sort.pval <- sort(pCADFtest.results$individual.tests[,1])
  N         <- length(sort.pval)
  critval   <- matrix(NA, N, length(alpha))
  for (i in 1:length(alpha))
  {
    critval[,i] <- (1:N)/N * alpha[i]
  }
  outcome   <- (apply((sort.pval <= critval), 2, "sum")==0)
  return(outcome)
}

