\name{Hartung}
\alias{Hartung}
\title{Hartung's combination test for dependent p-values}
\description{
  This function implements the procedure for combining dependent tests of significance
    proposed by Hartung (1999).
}
\usage{
  Hartung(p, lambda=rep(1,length(p)), kappa=0.2, alpha=0.10)
}
\arguments{
  \item{p}{the vector of p-values.}
  \item{lambda}{a vector of weights. It must be of the same length as p.}
  \item{kappa}{adjustment parameter. It can be either a positive scalar (0.2 is the default value) or 
		it can take the character value "formula". When \code{k = "formula"} is used, then it is computed as 
		in Hartung (1999), p. 853.}
  \item{alpha}{level for the 1-alpha confidence interval for rho (0.10 is the default).}
}
%\details{
%   ~~ If necessary, more details than the description above ~~
%}
\value{
  The function returns a list of class \code{"htest"} containing:
    \item{statistic}{the Ht test statistic.}
    \item{parameter}{the number of combined tests (p-values).}
    \item{p.value}{the p-value of the combination test.}
    \item{conf.int}{the confidence interval for the estimated correlation.}
    \item{estimate}{the estimated correlation.}
    \item{null.value}{the specified hypothesized value under the null.}
    \item{alternative}{a character string describing the alternative hypothesis.}
    \item{method}{a character string indicating the type of combination test.}
    \item{data.name}{a character string giving the name of the vector of p-values.}
}
\references{
    Hartung, J (1999). A Note on Combining Dependent Tests of Significance, \emph{Biometrical
    Journal}, \bold{41} (7), 849--855.
}
\author{Claudio Lupi}

%\note{} 

%\seealso{}
\examples{
  fake.pvalues <- runif(20)
  Hartung(fake.pvalues)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
