\name{pfisher}
\alias{pfisher}

\title{Fisher-type Tests for Combining p-Values}

\description{
A function implementing the first-generation combination procedures proposed by Maddala and Wu (1999) and Choi (2001).
}

\usage{
 pfisher(object, method = c("invchisq", "invnorm", "invlogit"), 
            N = NULL, exo = c("intercept", "none", "trend"), 
            lags = NULL)
}

\arguments{
  \item{object}{A data frame.}
  \item{method}{One of \code{"invchisq"}, \code{"invnorm"}, \code{"invlogit"}.}
  \item{N}{... .}
  \item{exo}{One of \code{"intercept"}, \code{"none"}, \code{"trend"}.}
  \item{lags}{The number of lags for the unit root tests.}
}

%\details{
%   ~~ If necessary, more details than the description above ~~
%}

\value{
  The function returns a list of class \code{"htest"} containing:
    \item{statistic}{the test statistic.}
    \item{parameter}{the number of combined tests (p-values).}
    \item{alternative}{a character string describing the alternative hypothesis.}
    \item{method}{a character string indicating the type of combination test.}
    \item{data.name}{a character string giving the name of the vector of p-values.}
    \item{p.value}{the p-value of the combination test.}
%    \item{conf.int}{the confidence interval for the estimated correlation.}
%    \item{estimate}{the estimated correlation.}
%    \item{null.value}{the specified hypothesized value under the null.}
}

\references{
 Choi I (2001). Unit Root Tests for Panel Data. \emph{Journal of International Money and Finance}, \bold{20}, 249--272.

 Fisher RA (1932). \emph{Statistical Methods for Research Workers}, 4th ed. Edinburgh: Oliver & Boyd.

 Maddala GS, Wu S (1998). A Comparative Study of Unit Root Tests With Panel Data and a New Simple Test. \emph{Oxford Bulletin of Economics and Statistics}, Special Issue, 631--652.
}

%\note{} 

%\seealso{}

\examples{
## compare Stata xtunitroot

data("G7")
y <- data.frame(split(G7$lrxrate, G7$country))

pfisher(y, exo = "intercept", lags = 10, N = Inf, method = "invchisq")
pfisher(y, exo = "intercept", lags = 10, N = Inf, method = "invnorm")
pfisher(y, exo = "intercept", lags = 10, N = Inf, method = "invlogit")

}

\keyword{htest}
