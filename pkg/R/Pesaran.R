Pesaran <- function(resids)
#
# This function computes Pesaran's (2004) test for cross-dependence in panels.
# Arguments: 
#		resids: TxN matrix of residuals from N single-equation models over N time series of length T
#
# Value:
#		a list of class ``htest'' containing
#		statistic: the test statistic
#		parameter: the length of time series (T) and the number of models (N)
#		p.value: the test p-value
#		conf.int: not used
#		estimate: not used
#		null.value: the specified hypothesized value under the null
#		alternative: a character string describing the alternative hypothesis
#		method: a character srting indicating the type of test
#		data.name: a character string giving the name of the data set
#
# Reference:
# Pesaran, M.H. (2004): "General Diagnostic Tests for Cross Section Dependence in Panels"
#			University of Cambridge, mimeo
#
{ 
  T <- dim(resids)[1]
  N <- dim(resids)[2]
  residCorr <- cor(resids, use="pairwise.complete.obs")
  # test statistic
  CD <- sqrt(2*T/N/(N-1)) * sum(residCorr - diag(N))/2
  # p.value
  p.value <- 2*(1-pnorm(abs(CD)))

  output <- list(statistic=c("CD"=CD),
	  parameter=c("N"=N, "T"=T),
	  p.value=p.value,
	  conf.int=NULL,
	  estimate=NULL,
	  null.value=c("CD"=0),
	  alternative="two.sided",
	  method="Pesaran's test for residuals cross-correlation",
	  data.name=deparse(substitute(resids)))

  class(output) <- "htest"

  return(output)
}
  