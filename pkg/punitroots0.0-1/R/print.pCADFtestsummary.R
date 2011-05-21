print.pCADFtestsummary <- function(x, ...)
{
	# x is an object of class `pCADFtestsummary'
	ttype <- "Panel Covariate Augmented DF test"
	if (nrow(x$test.summary)==2) ttype <- "Panel Augmented DF test"
	cat(ttype, "\n")
	cat("Correction for cross-correlation:", x$corr, "\n")
	cat("\n")
	print(x$individual.tests, ...)
	print(x$test.summary, ...)
}
