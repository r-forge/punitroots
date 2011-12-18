\name{phadri}
\alias{phadri}

\title{Hadri's Panel Stationarity Test}

\description{
A function implementing the first-generation combination procedure proposed by Hadri (2000).
}

\usage{
 phadri(object, exo = c("intercept", "trend"), kernel = c("Bartlett", "Quadratic Spectral", 
         "Truncated", "Parzen", "Tukey-Hanning"), bw = NULL, het = TRUE, ...)
}

\arguments{
  \item{object}{A data frame.}
  \item{exo}{One of \code{"intercept"}, \code{"trend"}.}
  \item{kernel}{One of ....}
  \item{bw}{Bandwidth for the kernel.}
  \item{het}{Logical. Cross-sectional heteroskedasticity?}
  \item{...}{Further arguments passed on to kernHAC.}
}

%\details{
%   ~~ If necessary, more details than the description above ~~
%}

\value{
  The function returns a list of class \code{"htest"} containing:
    \item{statistic}{the test statistic.}
    \item{parameter}{the value of the parameter.}
    \item{alternative}{a character string describing the alternative hypothesis.}
    \item{data.name}{a character string giving the name of the vector of p-values.}
    \item{method}{a character string indicating the type of combination test.}
    \item{p.value}{the p-value of the combination test.}
    \item{istat}{the test statistics for the individual series.}
    \item{ilrv}{the individual long-run variances.}
    \item{mlrv}{the mean of the long-run variances.}
}

\references{
 Hadri K (2000). Testing for Stationarity in Heterogeneous Panel Data. \emph{Econometrics Journal}, \bold{3}, 148--161.
}

%\note{} 

%\seealso{}

\examples{

data("G7")
g7 <- data.frame(split(G7$lrxrate, G7$country))

## Eviews does (het = TRUE)
phadri(g7, exo = "intercept", bw = 2, adjust = FALSE)
phadri(g7, exo = "intercept", bw = 6, adjust = FALSE)
phadri(g7, exo = "trend", bw = 2, adjust = FALSE)
phadri(g7, exo = "trend", bw = 6, adjust = FALSE)

## Eviews does (het = FALSE)
phadri(g7, exo = "intercept", bw = 2, adjust = FALSE, het = FALSE)
phadri(g7, exo = "intercept", bw = 6, adjust = FALSE, het = FALSE)
phadri(g7, exo = "trend", bw = 2, adjust = FALSE, het = FALSE)
phadri(g7, exo = "trend", bw = 6, adjust = FALSE, het = FALSE)

## Stata does (option robust)
phadri(g7, exo = "intercept", bw = 1, adjust = TRUE)

}

\keyword{htest}
