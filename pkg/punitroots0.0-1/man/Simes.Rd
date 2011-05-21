\name{Simes}
\alias{Simes}
\title{Simes' test for panel unit root}
\description{
  This function implements Simes' test applied to the panel unit root hypothesis (see Simes, 1986; Sarkar and Chang, 1997; Hanck 2008).
}
\usage{
  Simes(pCADFtest.results, alpha = 0.05)
}
\arguments{
  \item{pCADFtest.results}{an object of class pCADFtest.}
  \item{alpha}{the significance level (it can be passed as a vector).} 
}

\value{
    \item{outcome}{logical: it is a vector if alpha is a vector. TRUE if the test doesn't reject 
      at the specified significance level, FALSE otherwise.}
}
\references{
    Hanck, C (2008). An Intersection Test for Panel Unit Roots, Technische Universitaet Dortmund, mimeo.

    Sarkar, SK and Chang, C-K (1997). The Simes Method for Multiple Hypothesis Testing With Positively Dependent Test Statistics, \emph{Journal of the American Statistical Association}, \bold{92} (440), 1601--1608.

    Simes, RJ (1986). An Improved Bonferroni Procedure for Multiple Tests of Significance, \emph{Biometrika}, \bold{73} (3), 751--754.
}
\author{Claudio Lupi}

%\note{} 

%\seealso{}
\examples{
data("GDPseries")
Y <- log(GDPseries)
Demetrescuetal.test <- pCADFtest(Y, max.lag.y = 2, criterion = "AIC")
Simes(Demetrescuetal.test, c(0.01, 0.05, 0.10))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Simes test}
