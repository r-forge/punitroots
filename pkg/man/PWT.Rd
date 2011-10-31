\name{PWT}
\alias{PWT}
\title{Penn Word Tables}

\description{
Macro data on 29 countries for the years 1950 to 1992, obtained form the Penn World Table, Mark 5.}

\usage{data("PWT")
}

\format{
A data frame containing annual data for 29 countries from 1950 to 1992 with 5 variables, in long form.
 \describe{
    \item{id}{Country identifier, an integer.}
    \item{year}{Annual date of the observation.}
    \item{pop}{Population.}
    \item{rgdpl}{Laspeyres real GDP.}
    \item{rinv}{Laspeyres real investment.}
 }
}

\details{The data are from the Penn World Table (Mark 5), see Summers and Heston (1991). 
}


\source{
Journal of Applied Econometrics Data Archive for Pedroni (2007).

\url{http://qed.econ.queensu.ca/jae/datasets/pedroni001/}
}

\references{
Pedroni P (2007). Social Capital, Barriers to Production and Capital
Shares: Implications for the Importance of Parameter Heterogeneity from
a Nonstationary Panel Approach. \emph{Journal of Applied Econometrics},
\bold{22}(2), 429--451.

Summers R, Heston A (1991). The Penn World Table (Mark 5): An
Expanded Set of International Comparisons, 1950--1988. \emph{Quarterly Journal
of Economics}, \bold{106}, 327--368. 
}

\seealso{
}

\examples{
data("PWT")
library("CADFtest")

## selected values from Pedroni, ...

}

\keyword{datasets}
