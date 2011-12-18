## Hadri test (combining KPSS tests)

phadri <- function(object, exo = c("intercept", "trend"), 
            kernel = c("Bartlett", "Quadratic Spectral", 
            "Truncated", "Parzen", "Tukey-Hanning"), 
            bw = NULL, het = TRUE, ...) {

#require("sandwich")

data.name <- paste(deparse(substitute(object)))

#if(mode(lags) != "numeric") stop("'lags' must be a number")
#if(round(lags) != lags) stop("'lags' must be an integer")

kernel <- match.arg(kernel)
exo <- match.arg(exo)

nobs <- nrow(object)
nseries <- ncol(object)

trend <- 1:nobs

## internal function for kpss test
 kpss <- function(x, exo = exo, bw = bw, ...){
  if(exo == "intercept") lmobj <- lm(x ~ 1)
  if(exo == "trend")     lmobj <- lm(x ~ trend)
#  lmobj <- ifelse(exo == "intercept", lm(x ~ 1), lm(x ~ trend)) 
  u <- resid(lmobj)
  uu <- mean(cumsum(u)^2) / nobs
  ## warning: note def. of kernHAC!
  lrv <- kernHAC(lm(u ~ 1), prewhite = FALSE, bw = bw, 
                kernel = kernel, ...) * nobs             
  uu <- uu / lrv
  list(kpss = uu, lrv = lrv)
 }
  
 if(exo == "intercept") adj <- c(1/6, 1/45)
 if(exo == "trend")     adj <- c(1/15, 11/6300)

 ## individual KPSS statistics and long-run variances
  stati <- apply(object, 2, function(x) kpss(x, exo = exo, bw = bw, ...)$kpss)
  lrvi <- apply(object, 2, function(x) kpss(x, exo = exo, bw = bw, ...)$lrv)
  mlrv <- mean(lrvi)
 
 ## cross-sectional heteroskedasticity?
 if(het) {
  stat <- mean(stati)
  } else {
  stat <- mean(stati * lrvi) / mean(lrvi)
  }
 
 ## Hadri statistic 
  stat <- sqrt(nseries) * (stat - adj[1]) / sqrt(adj[2])
  names(stat) <- "H"
  pvalue <- pnorm(stat, lower.tail = FALSE)
  parameter <- NULL
  method <-  "Hadri panel stationarity test"

 result <- structure(list(statistic = stat,
                          parameter = parameter,
                          alternative = "at least one series has a unit root",
                          data.name = data.name,
                          method = method,
                          istat = stati,
                          ilrv = lrvi,
                          mlrv = mlrv,
                          p.value = pvalue),
                          class = "htest")
  
 ##result <- list(statistic = htest,
 #               call = cl,
 #               args = args,
 #               idres = idres,
 #               adjval = adjval)
 #class(result) <- "htest"
 result

}
