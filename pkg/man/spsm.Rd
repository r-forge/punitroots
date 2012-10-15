\name{spsm}
\alias{spsm}
\title{Sequential panel selection method}
\description{
  This function implements the sequential panel selection method advocated in Chortareas and Kapetanios, 2009.
}
\usage{
  spsm(pCADFtest.results, alpha = 0.05)
}
\arguments{
  \item{pCADFtest.results}{an object of class pCADFtest.}
  \item{alpha}{the significance level.} 
}

\value{
    \item{outcome}{Vector of identifiers of series considered to be I(0).}
}
\references{
    Chortareas, G and Kapetanios, G (2009). Getting PPP Right: Identifying Mean-reverting Real Exchange Rates in Panels, \emph{Journal of Banking & Finance}, \bold{33} (2), 390--404.
}
\author{Claudio Lupi}

%\note{} 

%\seealso{}
\examples{
data("OECDgdp")
Y <- log(OECDgdp)
Demetrescuetal.test <- pCADFtest(Y, max.lag.y = 2, criterion = "AIC")
spsm(Demetrescuetal.test, c(0.01, 0.05, 0.10))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Sequential panel test}
