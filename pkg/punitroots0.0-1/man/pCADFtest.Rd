\name{pCADFtest}
\alias{pCADFtest}
\title{Panel Covariate-Augmented Dickey Fuller (CADF) test for unit roots}
\description{
  This function implements the panel Covariate Augmented Dickey-Fuller (pCADF) test developed in
  Costantini and Lupi (2011). The panel unit root tests proposed in Choi (2001) and in
  Demetrescu et al. (2006) can also be performed using this function.
}
\usage{
pCADFtest(Y, X=NULL, covariates=NULL, crosscorr=0.10, type="trend", 
     data=list(), max.lag.y=1, min.lag.X=0, max.lag.X=0, 
     dname=NULL, criterion=c("none", "BIC", "AIC", "HQC", 
     "MAIC"), ...)
}

\arguments{
  \item{Y}{a multiple time series or a \eqn{T \times N} matrix. It contains the series to be tested. 
	    The series may have different length.}
  \item{X}{a vector, a matrix, or a vector time series of stationary covariates. If no \code{X} is specified,
  then the tests proposed by Choi (2001) and Demetrescu et al. (2006) are performed. As an alternative, the pCADF
  test (Costantini and Lupi 2011) can also be performed with the stationary covariate(s) derived from \code{Y}.} 
  \item{covariates}{a character or a vector of scalars containing integers from 1 to N. The default is \code{NULL}. 
  When \code{covariates = NULL} then tests proposed in Choi (2001) or Demetrescu et al. (2006) are performed 
  (no stationary covariates). If \code{covariates = "PC"}, the stationary covariate is computed as the difference 
  of the first principal component of \code{Y} and the same covariate is used for all the individual tests. 
  If \code{covariates = "DY"}, each individual test is carried out using the average of the other differenced \code{Y}. 
  See Costantini and Lupi (2011) for details. Otherwise, \code{covariates} must be a vector of length equal to the number
  of columns of \code{X}. The first number will indicate which column of \code{Y} is associated to the first stationary
  covariate represented by the first column of \code{X}; the second number identifies the correspondence between a 
  column of \code{Y} with the second column of \code{X} and so on.}
  \item{crosscorr}{a real scalar between 0 and 1. It is the threshold of the p-value of Pesaran's test for cross-correlation.
  If the actual test p-value is lower than \code{crosscorr}, then the correction proposed by Hartung (1999) is applied.
  if \code{crosscorr = 0} the original p-value combination test developed by Choi (2001) for independent series is 
  carried out. If \code{crosscorr = 1} then Hartung's correction is always applied as in Demetrescu et al. (2006).} 
  \item{type}{a character or a \eqn{N}-vector of characters. It defines the deterministic kernel to be used in the tests.
  It accepts the values used in package \code{urca}. It specifies if the underlying model must be with linear trend 
  ("trend", the default), with constant ("drift") or without constant ("none"). When a character is passed to the procedure,
  then the same deterministic kernel is used for all the tests. }
  \item{data}{not used.}
  \item{max.lag.y}{maximum number of lags allowed for the lagged differences of the variable to be tested. 
  Both a scalar integer or a \eqn{N}-vector of integers can be used. When using a scalar, the same maximum lag is 
  used for all the series. Different maximum lags can be used for each series by defining a \eqn{N}-vector of integers.}
  \item{min.lag.X}{an integer scalar or an vector of integers. Same as \code{max.lag.y}. If negative it is the maximum 
  lead allowed for the covariates. If zero, it is the minimum lag allowed for the covariates. When more than one
  covariate is used, the same \code{min.lag.X} is used for all the covariates.}
  \item{max.lag.X}{an integer scalar or an vector of integers. Maximum lag allowed for the covariates. Same as 
  \code{min.lag.X}.}
  \item{dname}{NULL or character. It can be used to give a special name to the model. 
  If the NULL default is accepted and the model is specified using a formula notation, then
  dname is computed according to the used formula.}
  \item{criterion}{it can be either \code{"none"} (the default), \code{"BIC"}, \code{"AIC"},
  \code{"HQC"} or \code{"MAIC"}. If \code{criterion="none"}, no automatic model selection 
  is performed. Otherwise, automatic model selection is performed using the specified 
  criterion. In this case, the max and min orders serve as upper and lower bounds in the 
  model selection.}          
  \item{...}{Extra arguments that can be set to use special kernels, prewhitening, etc. in the estimation of 
  \eqn{\rho^2}. A Quadratic kernel with a VAR(1) prewhitening is the default choice. To set 
  these extra arguments to different values, see \code{kernHAC} in package \code{sandwich} 
  (Zeileis, 2004, 2006). Other arguments can be passed also
  to the procedure that performs Hartung's correction.}
}
%\details{
%   ~~ If necessary, more details than the description above ~~
%}
\value{
  The function returns an object of class \code{c("pCADFtest", "htest")} containing:
  \item{statistic}{the test statistic.}
  \item{parameter}{the estimated nuisance parameter \eqn{\rho^2} (see Hansen, 1995, p. 1150).}
  \item{method}{the test performed: it can be either \code{ADF} or \code{CADF}.}
  \item{p.value}{the p-value of the test.}
  \item{corr}{logical. \code{TRUE} if Hartung correction has been applied, \code{FALSE} otherwise.}
  \item{individual.tests}{a \eqn{N \times 5} matrix containing the values of the p.value, 
  \eqn{\rho^2}, the orders \eqn{p}, \eqn{q_1} and \eqn{q_2} of each single test on each of the
  \eqn{N} time series.}
  \item{Pesaran}{the outcome of Pesaran's test for cross-dependence.}
}
\references{
    Choi I (2001). Unit Root Tests for Panel Data, \emph{Journal of International Money and 
    Finance}, \bold{20}(2), 249--272.

    Costantini M, Lupi C, (2011). A Simple Panel-CADF Test for Unit Roots. Economics Series n. 261, 
    Institute for Advanced Studies, Vienna.

    Hansen BE (1995). Rethinking the Univariate Approach to Unit Root Testing: Using
    Covariates to Increase Power, \emph{Econometric Theory}, \bold{11}(5), 1148--1171.

    Hartung J (1999). A Note on Combining Dependent Tests of Significance, 
    \emph{Biometrical Journal}, \bold{41}(7), 849--855.

    Lupi C (2009). Unit Root CADF Testing with R, \emph{Journal of Statistical Software},
    \bold{32}(2), 1--19. \url{http://www.jstatsoft.org/v32/i02/}

    Pesaran MH (2004). General Diagnostic Tests for Cross Section Dependence in Panels, 
    University of Cambridge, mimeo.

    Zeileis A (2004). Econometric Computing with HC and HAC
    Covariance Matrix Estimators, \emph{Journal of Statistical Software}, \bold{11}(10),
    1--17. \url{http://www.jstatsoft.org/v11/i10/}

    Zeileis A (2006). Object-Oriented Computation of Sandwich
    Estimators, \emph{Journal of Statistical Software}, \bold{16}(9), 1--16.
    \url{http://www.jstatsoft.org/v16/i09/}.
}
\author{Claudio Lupi}

\seealso{\code{fUnitRoots}, \code{urca}}
\examples{
data("GDPseries")
Y <- log(GDPseries)
Demetrescuetal.test <- pCADFtest(Y, max.lag.y = 2, criterion = "AIC")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{htest}
