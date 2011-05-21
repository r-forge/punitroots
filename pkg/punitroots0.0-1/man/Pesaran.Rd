\name{Pesaran}
\alias{Pesaran}
\title{Pesaran's test for cross-correlation among panel units.}
\description{
  This function implements the procedure for testing for the presence of 
  cross-correlation among panel units using the test proposed in Pesaran (2004).
}
\usage{
  Pesaran(resids)
}
\arguments{
  \item{resids}{a \eqn{T \times N} matrix of residuals from \eqn{N} single-equation models 
		    over \eqn{N} time series of length \eqn{T}.}
}
\details{
This function should be applied to residuals from individual tests regressions.
}
\value{
  The function returns a list of class \code{"htest"} containing:
    \item{statistic}{the test statistic.}
    \item{parameter}{the number \eqn{N} of time series and their length \eqn{T}.}
    \item{p.value}{the p-value of the test.}
    \item{null.value}{the specified hypothesized value under the null.}
    \item{alternative}{a character string describing the alternative hypothesis.}
    \item{method}{a character string indicating the type of test.}
    \item{data.name}{a character string giving the name of the vector of residuals.}
}
\references{
    Pesaran, MH (2004). General Diagnostic Tests for Cross Section Dependence in Panels, 
    Department of Applied Economics, University of Cambridge, mimeo.
}
\author{Claudio Lupi}

%\note{} 

%\seealso{}
\examples{
  fake.resids <- matrix(rnorm(1000),100,10)
  Pesaran(fake.resids)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
