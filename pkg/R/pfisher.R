## Fisher-type tests (combining p-values)

pfisher <- function(object, 
            method = c("invchisq", "invnorm", "invlogit"), 
            N = NULL, exo = c("intercept", "none", "trend"), 
            lags = NULL) {

#require("fUnitRoots")

data.name <- paste(deparse(substitute(object)))

if(mode(lags) != "numeric") stop("'lags' must be a number")
if(round(lags) != lags) stop("'lags' must be an integer")

exo <- match.arg(exo)
method <- match.arg(method)

## quick fix
 if(exo == "intercept") adfarg <- "c"
 if(exo == "none")      adfarg <- "nc"
 if(exo == "trend")     adfarg <- "ct"

 
## ADFs and their p-values
 tstats <- apply(object, 2, function(x) unitrootTest(x, lags = lags, type = adfarg)@test$statistic)

 pvals <- sapply(tstats, function(x) punitroot(x, N = N, trend = adfarg, statistic = "t"))
 n <- length(pvals)
 
## inverse chi-squared aka Fisher aka 'madwu' 
 if(method == "invchisq"){
  stat <-  -2*sum(log(pvals))
  names(stat) <- "P"
  pvalue <- pchisq(stat, df = 2*n, lower.tail = FALSE)
  parameter <- c(df = 2 * n)
  method <-  "Inverse chi-square test (Maddala and Wu)"
 }
 
## inverse normal from Choi 2001 
 if(method == "invnorm"){
  stat <-  sum(qnorm(pvals))/sqrt(n)
  names(stat) <- "Norm"
  pvalue <- pnorm(stat, lower.tail = TRUE)
  parameter <- NULL
  method <-  "Inverse normal test (Choi)"
 }
 
## inverse logit from Choi 2001 
 if(method == "invlogit"){
  stat <-  sum(log(pvals / (1 - pvals)))
  k <- 3 * (5*n + 4) / pi^2 / n / (5*n + 2) 
  stat <- sqrt(k) * stat
  names(stat) <- "L*"
  pvalue <- pt(stat, df = 5*n+4, lower.tail = TRUE)
  parameter <- c(df = 5*n+4)
  method <-  "Inverse logit test (Choi)"
 }

 result <- structure(list(statistic = stat,
                          parameter = parameter,
                          alternative = "at least one series is stationary",
                          data.name = data.name,
                          method = method,
                          p.value = pvalue),
                     class = "htest")
  
 result
 
}

