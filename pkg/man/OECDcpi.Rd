\name{OECDcpi}
\alias{OECDcpi}
\title{Inflation in OECD Countries}

\description{
Monthly consumer price index data for 13 OECD countries.}

\usage{data("OECDcpi")
}

\format{
A monthly multiple time series from 1957(1) to 1994(10) with 13 variables.
 \describe{
    \item{US}{Consumer price index for the USA.}
    \item{UK}{Consumer price index for the UK.}
    \item{Belgium}{Consumer price index for Belgium.}
    \item{France}{Consumer price index for France.}
    \item{Germany}{Consumer price index for Germany.}
    \item{Italy}{Consumer price index for Italy.}
    \item{Luxemburg}{Consumer price index for Luxemburg.}
    \item{Netherlands}{Consumer price index for the Netherlands.}
    \item{Norway}{Consumer price index for Norway.}
    \item{Canada}{Consumer price index for Canada.}
    \item{Japan}{Consumer price index for Japan.}
    \item{Finland}{Consumer price index for Finland.}
    \item{Spain}{Consumer price index for Spain.}
 }
}

\details{
}


\source{
Journal of Applied Econometrics Data Archive for Culver and Papell (1997).

\url{http://qed.econ.queensu.ca/jae/datasets/culver001/}
}

\references{
Culver SE, Papell DH (1997). Is There a Unit Root in the
Inflation Rate?  Evidence from Sequential Break and Panel Data Models. 
\emph{Journal of Applied Econometrics}, \bold{12}(4), 435--444.
}

\seealso{
}

\examples{
library("CADFtest")

## selected values from Culver and Papell, Table I
CADFtest(diff(log(OECDcpi[, "Belgium"])), type = "drift", data = OECDcpi, max.lag.y = 12)
CADFtest(diff(log(OECDcpi[, "UK"])), type = "drift", data = OECDcpi, max.lag.y = 13)
CADFtest(diff(log(OECDcpi[, "US"])), type = "drift", data = OECDcpi, max.lag.y = 16)


}

\keyword{datasets}
