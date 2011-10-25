\name{OECDrealex}
\alias{OECDrealex}
\title{OECD Real Exchange Rates}

\description{
Quarterly real exchange rates against the US dollar for 17 OECD countries.}

\usage{data("OECDrealex")
}

\format{
A quarterly multiple time series from 1973(1) to 1998(4) with 17 variables.
 \describe{
    \item{Australia}{Real exchange rate for Australia.}
    \item{New Zealand}{Real exchange rate for New Zealand.}
    \item{Austria}{Real exchange rate for Austria.}
    \item{Belgium}{Real exchange rate for Belgium.}
    \item{Canada}{Real exchange rate for Canada.}
    \item{Denmark}{Real exchange rate for Denmark.}    
    \item{Finland}{Real exchange rate for Finland.}
    \item{France}{Real exchange rate for France.}    
    \item{Germany}{Real exchange rate for Germany.}
    \item{Italy}{Real exchange rate for Italy.}
    \item{Japan}{Real exchange rate for Japan.}
    \item{Netherlands}{Real exchange rate for the Netherlands.}
    \item{Norway}{Real exchange rate for Norway.}
    \item{Spain}{Real exchange rate for Spain.}
    \item{Sweden}{Real exchange rate for Sweden.}
    \item{Switzerland}{Real exchange rate for Switzerland.}
    \item{UK}{Real exchange rate for the UK.}   
 }
}

\details{The real exchange rates are measured in logarithms, such that the series of
interest for country [i] is, at time [t],

   Y_it = S_it + P_USt - P_it,
   
where S_it is the logarithm of the units of country [i]'s currency per US
dollar, and P[USt] and P[it] are the logarithms of consumer price indices in
the US and country [i], respectively. The nominal exchange rate and consumer
price index data for the period under investigation are obtained from the
OECD Main Economic Indicators Statistical Compendium 2000.
}

\source{
Journal of Applied Econometrics Data Archive for Pesaran (2007).

\url{http://qed.econ.queensu.ca/jae/datasets/pesaran002/}
}

\references{
Pesaran MH (2007). A Simple Panel Unit Root Test in the Presence of Cross
Section Dependence, \emph{Journal of Applied Econometrics}, \bold{22}(2), 265--312.
}

\seealso{
}

\examples{
data("OECDrealex")
plot(OECDrealex[,1:4])

## try: selected values from Pesaran, Table X


}

\keyword{datasets}
