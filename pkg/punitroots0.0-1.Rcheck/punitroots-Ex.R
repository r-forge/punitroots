pkgname <- "punitroots"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('punitroots')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Hartung")
### * Hartung

flush(stderr()); flush(stdout())

### Name: Hartung
### Title: Hartung's combination test for dependent p-values
### Aliases: Hartung
### Keywords: htest

### ** Examples

  fake.pvalues <- runif(20)
  Hartung(fake.pvalues)



cleanEx()
nameEx("Pesaran")
### * Pesaran

flush(stderr()); flush(stdout())

### Name: Pesaran
### Title: Pesaran's test for cross-correlation among panel units.
### Aliases: Pesaran
### Keywords: htest

### ** Examples

  fake.resids <- matrix(rnorm(1000),100,10)
  Pesaran(fake.resids)



cleanEx()
nameEx("Simes")
### * Simes

flush(stderr()); flush(stdout())

### Name: Simes
### Title: Simes' test for panel unit root
### Aliases: Simes
### Keywords: Simes test

### ** Examples

data("GDPseries")
Y <- log(GDPseries)
Demetrescuetal.test <- pCADFtest(Y, max.lag.y = 2, criterion = "AIC")
Simes(Demetrescuetal.test, c(0.01, 0.05, 0.10))



cleanEx()
nameEx("pCADFtest")
### * pCADFtest

flush(stderr()); flush(stdout())

### Name: pCADFtest
### Title: Panel Covariate-Augmented Dickey Fuller (CADF) test for unit
###   roots
### Aliases: pCADFtest
### Keywords: ts htest

### ** Examples

data("GDPseries")
Y <- log(GDPseries)
Demetrescuetal.test <- pCADFtest(Y, max.lag.y = 2, criterion = "AIC")



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
