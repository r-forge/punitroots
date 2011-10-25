\name{Earnings}
\alias{EarningsCLG}
\alias{EarningsHSD}
\alias{EarningsHSG}

\docType{data}

\title{Time Series of Individual Earnings}

\description{
Real earnings (in logarithms) for a sample of 181 individuals from the Panel Study of Income Dynamics (PSID).
}

\usage{
data("EarningsCLG")
data("EarningsHSD")
data("EarningsHSG")
}

\format{
Data are annual multiple time series from 1971 to 1992 for 36 (HSD), 87 (HSG), and 58 (CLG) individuals, respectively. 
}

\details{
The data set (log of real earnings) is based on the PSID data used by Meghir
and Pistaferri (2004). The appendix of that paper contains 
step-by-step details on sample selection. Pesaran (2007) further selects the individuals 
having continuous 22 observations from 1971--1992, giving 181
individuals in total. Following Meghir and Pistaferri, he also categorize the individuals
into three education groups: High School Dropouts (HSD, 36 individuals),
High School Graduates (HSG, 87 individuals) and College Graduates (CLG, 58
individuals). These are the three data sets provided here.
}

\source{
Journal of Applied Econometrics Data Archive for Pesaran (2007).

\url{http://qed.econ.queensu.ca/jae/datasets/pesaran002/}
}

\references{
Meghir C, Pistaferri L (2004). Income Variance Dynamics and Heterogeneity. \emph{Econometrica}, \bold{72}, 1--32.

Pesaran MH (2007). A Simple Panel Unit Root Test in the Presence of Cross
Section Dependence, \emph{Journal of Applied Econometrics}, \bold{22}(2), 265--312.
}


\examples{
data("EarningsHSG")
plot(EarningsHSG[, 1:10])

## try: selected values from Pesaran, Table XIII


}



\keyword{datasets}
