###################################################
### chunk number 1: 
###################################################
#line 57 "panelUnitRootWithR.Rnw"
options(prompt = "R> ", continue="+ ", useFancyQuotes=FALSE)
## R version to be used in Section "Computational details"
R.V <- R.Version()$version.string
R.V <- substr(R.V, 3, nchar(R.V))
library('plm')
library('punitroots')


###################################################
### chunk number 2: 
###################################################
#line 201 "panelUnitRootWithR.Rnw"
library("plm")
library("punitroots")
data("unempseries")


###################################################
### chunk number 3: 
###################################################
#line 209 "panelUnitRootWithR.Rnw"
plot(unempseries, main="International unemployment rates series")


###################################################
### chunk number 4: 
###################################################
#line 219 "panelUnitRootWithR.Rnw"
u <- log(unempseries[,-8]/(100-unempseries[,-8]))
colnames(u) <- colnames(unempseries)[-8]


###################################################
### chunk number 5: 
###################################################
#line 233 "panelUnitRootWithR.Rnw"
my.u <- as.matrix(window(u, start = c(1979,4), end = c(2010,1)))
LLC <- purtest(my.u, test = "levinlin", exo = "intercept", 
	lags = "AIC", pmax = 5)
summary(LLC)


###################################################
### chunk number 6: 
###################################################
#line 242 "panelUnitRootWithR.Rnw"
IPS <- purtest(my.u, test = "ips", exo = "intercept", 
	lags = "AIC", pmax = 5)
summary(IPS)


###################################################
### chunk number 7: 
###################################################
#line 256 "panelUnitRootWithR.Rnw"
Choi <- pCADFtest(Y=u, type = "drift", max.lag.y = 5, criterion = "AIC")
summary(Choi)


###################################################
### chunk number 8: 
###################################################
#line 263 "panelUnitRootWithR.Rnw"
pCADF.PC <- pCADFtest(Y=u, covariates = "PC", max.lag.y = 5, max.lag.X = 5, 
	type="drift", criterion = "AIC")
summary(pCADF.PC)


###################################################
### chunk number 9: 
###################################################
#line 271 "panelUnitRootWithR.Rnw"
data("GDPseries")
X.GDP <- diff(log(GDPseries[,-8]))
pCADF.X <- pCADFtest(Y=u, X=X.GDP, covariates=1:ncol(u), 
	type="drift", max.lag.y = 5, max.lag.X = 5, criterion = "AIC")
summary(pCADF.X)


###################################################
### chunk number 10: 
###################################################
#line 286 "panelUnitRootWithR.Rnw"
Simes(Choi)


###################################################
### chunk number 11: 
###################################################
#line 290 "panelUnitRootWithR.Rnw"
Simes(pCADF.X)


###################################################
### chunk number 12: 
###################################################
#line 294 "panelUnitRootWithR.Rnw"
Simes(pCADF.X, alpha=c(0.01, 0.05, 0.10))


