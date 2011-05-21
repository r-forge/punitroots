summary.pCADFtest <- function(object, ...)
{
  # object is an object of class pCADFtest
  ind.tests <- object$individual.tests
  rnames <- 
    c("test statistic:          ",
      "average estimated rho^2: ",
      "p-value:                 ")

  cnames <- "Panel-CADF test"

  if (is.na(sum(ind.tests[,2])))
  {
    rnames <- rnames[c(1,3)]
    cnames <- "Panel-ADF test"
    ind.tests <- ind.tests[,c(1,3)]
  }

  test.summary <- matrix(NA,length(rnames), 1, dimnames=list(rnames,cnames))

  test.summary[1] <- object$statistic
  test.summary[2] <- object$parameter
  test.summary[3-(3-length(rnames))] <- object$p.value

  pCADFtestsummary <- list(individual.tests=ind.tests,
			  test.summary=test.summary,
			  corr=object$corr)

  class(pCADFtestsummary) <- c("pCADFtestsummary")  
  return(pCADFtestsummary)
}
