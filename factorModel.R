#============================================
# factorModels.r
# Examples for Scottish Financial Risk Academy Factor Model tutorial
# author: Eric Zivot
# created: January 10, 2011
# updated: March 14, 2011
#
# comments: Examples follow chapter 11 in Zivot and Wang (2006)

# set output options
options(width = 70, digits=4)

# load required packages
library(ellipse)
library(fEcofin)                # various data sets
library(PerformanceAnalytics)   # performance and risk analysis functions
library(zoo)
library(readxl)
library(writexl)

################################################################################
# Macroeconomic Factor Models
################################################################################

##
## Single Index Model
##

# load Berndt Data
#data(berndtInvest)
?read_excel
retdata = read_excel("berndt.xlsx")
retdata
str(retdata)
# create data frame with dates as rownames
# berndt.df = retdata[, -1]
#rownames(berndt.df) = as.character(berndtInvest[, 1])
#colnames(berndt.df)

##
## use multivariate regression and matrix algebra
##

returns.mat = as.matrix(retdata[, c(-1,-11, -18)])
market.mat = as.matrix(retdata[,11, drop=F])
n.obs = nrow(returns.mat)
X.mat = cbind(rep(1,n.obs),market.mat)
colnames(X.mat)[1] = "intercept"
XX.mat = crossprod(X.mat)

# multivariate least squares
G.hat = solve(XX.mat)%*%crossprod(X.mat,returns.mat)
# can also use solve(qr(X.mat), returns.mat)
beta.hat = G.hat[2,]
E.hat = returns.mat - X.mat%*%G.hat
diagD.hat = diag(crossprod(E.hat)/(n.obs-2))
# compute R2 values from multivariate regression
sumSquares = apply(returns.mat, 2, function(x) {sum( (x - mean(x))^2 )})
R.square = 1 - (n.obs-2)*diagD.hat/sumSquares
R.square
# print and plot results
cbind(beta.hat, diagD.hat, R.square)

par(mfrow=c(1,2))
barplot(beta.hat, horiz=T, main="Beta values", col="blue", cex.names = 0.75, las=1)
barplot(R.square, horiz=T, main="R-square values", col="blue", cex.names = 0.75, las=1)
par(mfrow=c(1,1))

# compute single index model covariance/correlation matrices
cov.si = as.numeric(var(market.mat))*beta.hat%*%t(beta.hat) + diag(diagD.hat)
cor.si = cov2cor(cov.si)
# plot correlations using plotcorr() from ellipse package
rownames(cor.si) = colnames(cor.si)
ord <- order(cor.si[1,])
ordered.cor.si <- cor.si[ord, ord]
plotcorr(ordered.cor.si, col=cm.colors(11)[5*ordered.cor.si + 6])
# compute global min variance portfolio
# use single index covariance
w.gmin.si = solve(cov.si)%*%rep(1,nrow(cov.si))
w.gmin.si = w.gmin.si/sum(w.gmin.si)
colnames(w.gmin.si) = "single.index"
# use sample covariance
w.gmin.sample = solve(var(returns.mat))%*%rep(1,nrow(cov.si))
w.gmin.sample = w.gmin.sample/sum(w.gmin.sample)
colnames(w.gmin.sample) = "sample"
cbind(w.gmin.si, sample = w.gmin.sample)

par(mfrow=c(2,1))
barplot(t(w.gmin.si), horiz=F, main="Single Index Weights", col="blue", cex.names = 0.75, las=2)
barplot(t(w.gmin.sample), horiz=F, main="Sample Weights", col="blue", cex.names = 0.75, las=2)
par(mfrow=c(1,1))

# compare means and sd values on global min variance portfolios
mu.vals = colMeans(returns.mat)
mu.gmin.si = as.numeric(crossprod(w.gmin.si, mu.vals))
sd.gmin.si = as.numeric(sqrt(t(w.gmin.si)%*%cov.si%*%w.gmin.si))
mu.gmin.sample = as.numeric(crossprod(w.gmin.sample, mu.vals))
sd.gmin.sample = as.numeric(sqrt(t(w.gmin.sample)%*%var(returns.mat)%*%w.gmin.sample))
cbind(mu.gmin.si,mu.gmin.sample, sd.gmin.si, sd.gmin.sample)

##
## use lm function to compute single index model regressions for each asset
##

asset.names = colnames(returns.mat)
asset.names

# initialize list object to hold regression objects
reg.list = list()

# loop over all assets and estimate time series regression
i = "CITCRP"
for (i in asset.names) {
  reg.df = retdata[, c(i, "MARKET")]
  si.formula = as.formula(paste(i,"~", "MARKET", sep=" "))
  reg.list[[i]] = lm(si.formula, data=reg.df)
}

# examine the elements of reg.list  - they are lm objects!
names(reg.list)
class(reg.list$CITCRP)
reg.list$CITCRP
summary(reg.list$CITCRP)

# plot actual vs. fitted over time
# use chart.TimeSeries() function from PerformanceAnalytics package
dataToPlot = cbind(fitted(reg.list$CITCRP), retdata$CITCRP)
colnames(dataToPlot) = c("Fitted","Actual")
dataToPlot.xts<-as.xts(dataToPlot, order.by = retdata$date)
chart.TimeSeries(dataToPlot.xts, main="Single Index Model for CITCRP",
                 colorset=c("black","blue"), legend.loc="bottomleft")


# scatterplot of the single index model regression
plot(retdata$MARKET, retdata$CITCRP, main="SI model for CITCRP",
     type="p", pch=16, col="blue",
     xlab="MARKET", ylab="CITCRP")
abline(h=0, v=0)
abline(reg.list$CITCRP, lwd=2, col="red")

## extract beta values, residual sd's and R2's from list of regression objects
## brute force loop
reg.vals = matrix(0, length(asset.names), 3)
rownames(reg.vals) = asset.names
colnames(reg.vals) = c("beta", "residual.sd", "r.square")
for (i in names(reg.list)) {
  tmp.fit = reg.list[[i]]
  tmp.summary = summary(tmp.fit)
  reg.vals[i, "beta"] = coef(tmp.fit)[2]
  reg.vals[i, "residual.sd"] = tmp.summary$sigma
  reg.vals[i, "r.square"] = tmp.summary$r.squared
}
reg.vals

# alternatively use R apply function for list objects - lapply or sapply
extractRegVals = function(x) {
  # x is an lm object
  beta.val = coef(x)[2]
  residual.sd.val = summary(x)$sigma
  r2.val = summary(x)$r.squared
  ret.vals = c(beta.val, residual.sd.val, r2.val)
  names(ret.vals) = c("beta", "residual.sd", "r.square")
  return(ret.vals)
}
reg.vals = sapply(reg.list, FUN=extractRegVals)
t(reg.vals)

################################################################################
# Fundamental Factor Models
################################################################################

# continue to use Berndt data for illustration of industry factor model

##
## industry factor model
##

# create loading matrix B for industry factor model
n.stocks = ncol(returns.mat)
tech.dum = oil.dum = other.dum = matrix(0,n.stocks,1)
rownames(tech.dum) = rownames(oil.dum) = rownames(other.dum) = asset.names
tech.dum[c(4,5,9,13),] = 1
oil.dum[c(3,6,10,11,14),] = 1
other.dum = 1 - tech.dum - oil.dum
B.mat = cbind(tech.dum,oil.dum,other.dum)
colnames(B.mat) = c("TECH","OIL","OTHER")
# show the factor sensitivity matrix
B.mat
colSums(B.mat)

# returns.mat is T x N matrix, and fundamental factor model treats R as N x T.
returns.mat = t(returns.mat)
# Step 1: Estimate OLS F.hat ----
  # multivariate OLS regression to estimate K x T matrix of factor returns  (K=3)
F.hat = solve(crossprod(B.mat))%*%t(B.mat)%*%returns.mat
  # rows of F.hat are time series of estimated industry factors (K X T)
F.hat.df<-data.frame(t(F.hat))
F.hat.df$date<-as.Date(retdata$date)
#
library(reshape2)
  # plot muliple time series using ggplot 
F.hat.df %>% melt("date") %>% 
  ggplot(aes(x = date, y = value, group=variable,color=variable)) +
    geom_line() +
    scale_x_date()

# Compute residual variance from OLS regression ---- 
  # compute N x T matrix of industry factor model residuals
E.hat = returns.mat - B.mat%*%F.hat
  # compute residual variances from time series of errors
diagD.hat = apply(E.hat, 1, var)
Dinv.hat = diag(diagD.hat^(-1))

# Step 2: Run multivariate FGLS regression to estimate K x T matrix of factor returns ----
H.hat = solve(t(B.mat)%*%Dinv.hat%*%B.mat)%*%t(B.mat)%*%Dinv.hat
colnames(H.hat) = asset.names
# note: rows of H sum to one so are weights in factor mimicking portfolios
F.hat.gls = H.hat%*%returns.mat
# show gls factor weights
t(H.hat)
colSums(t(H.hat))

# compare OLS and GLS fits
F.hat.gls.zoo = zoo(t(F.hat.gls), as.Date(colnames(F.hat.gls)))
par(mfrow=c(3,1))
plot(merge(F.hat.zoo[,1], F.hat.gls.zoo[,1]), plot.type="single",
     main = "OLS and GLS estimates of TECH factor",
     col=c("black", "blue"), lwd=2, ylab="Return")
legend(x = "bottomleft", legend=c("OLS", "GLS"), col=c("black", "blue"), lwd=2)
abline(h=0)

plot(merge(F.hat.zoo[,2], F.hat.gls.zoo[,2]), plot.type="single",
     main = "OLS and GLS estimates of OIL factor",
     col=c("black", "blue"), lwd=2, ylab="Return")
legend(x = "bottomleft", legend=c("OLS", "GLS"), col=c("black", "blue"), lwd=2)
abline(h=0)

plot(merge(F.hat.zoo[,3], F.hat.gls.zoo[,3]), plot.type="single",
     main = "OLS and GLS estimates of OTHER factor",
     col=c("black", "blue"), lwd=2, ylab="Return")
legend(x = "bottomleft", legend=c("OLS", "GLS"), col=c("black", "blue"), lwd=2)
abline(h=0)
par(mfrow=c(1,1))

# compute sample covariance matrix of estimated factors

cov.ind = B.mat%*%var(t(F.hat.gls))%*%t(B.mat) + diag(diagD.hat)
cor.ind = cov2cor(cov.ind)
# plot correlations using plotcorr() from ellipse package
rownames(cor.ind) = colnames(cor.ind)
ord <- order(cor.ind[1,])
ordered.cor.ind <- cor.ind[ord, ord]
plotcorr(ordered.cor.ind, col=cm.colors(11)[5*ordered.cor.ind + 6])

# compute industry factor model R-square values
r.square.ind = 1 - diagD.hat/diag(cov.ind)
ind.fm.vals = cbind(B.mat, sqrt(diag(cov.ind)), sqrt(diagD.hat), r.square.ind)
colnames(ind.fm.vals) = c(colnames(B.mat), "fm.sd", "residual.sd", "r.square")
ind.fm.vals

# compute global minimum variance portfolio
w.gmin.ind = solve(cov.ind)%*%rep(1,nrow(cov.ind))
w.gmin.ind = w.gmin.ind/sum(w.gmin.ind)
t(w.gmin.ind)

# compare weights with weights from sample covariance matrix
par(mfrow=c(2,1))
barplot(t(w.gmin.ind), horiz=F, main="Industry FM Weights", col="blue", cex.names = 0.75, las=2)
barplot(t(w.gmin.sample), horiz=F, main="Sample Weights", col="blue", cex.names = 0.75, las=2)
par(mfrow=c(1,1))

# compare means and sd values on global min variance portfolios
mu.gmin.ind = as.numeric(crossprod(w.gmin.ind, mu.vals))
sd.gmin.ind = as.numeric(sqrt(t(w.gmin.ind)%*%cov.ind%*%w.gmin.ind))
cbind(mu.gmin.sample,mu.gmin.sample, sd.gmin.ind, sd.gmin.sample)

################################################################################
# Statistical Factor Models
################################################################################

# continue to use Berndt data
returns.mat = as.matrix(berndt.df[, c(-10, -17)])

#
# Traditional factor analysis
#

#
# principal component analysis
#

# use R princomp() function for principal component analysis
pc.fit = princomp(returns.mat)
class(pc.fit)
names(pc.fit)

pc.fit
summary(pc.fit)
plot(pc.fit)
loadings(pc.fit)
pc.fit$loadings

# pc factors are in the scores component. Note these scores are based on
# centered data
head(pc.fit$scores[, 1:4])
# time series plot of principal component factors
chart.TimeSeries(pc.fit$scores[, 1, drop=FALSE], colorset="blue")

# compare with direct eigen-value analysis
# notice the sign change on the first set of loadings
eigen.fit = eigen(var(returns.mat))
names(eigen.fit)
names(eigen.fit$values) = rownames(eigen.fit$vectors) = asset.names
cbind(pc.fit$loadings[,1:2], eigen.fit$vectors[, 1:2])
# compute uncentered pc factors from eigenvectors and return data
pc.factors.uc = returns.mat %*% eigen.fit$vectors
colnames(pc.factors.uc) = paste(colnames(pc.fit$scores),".uc",sep="")
# compare centered and uncentered scores. Note sign change on first factor
cbind(pc.fit$scores[,1,drop=F], -pc.factors.uc[,1,drop=F])
chart.TimeSeries(cbind(pc.fit$scores[,1,drop=F], -pc.factors.uc[,1,drop=F]),
                 main="Centered and Uncentered Principle Component Factors",
                 legend.loc="bottomleft")

# compare first pc factor with market return
chart.TimeSeries(cbind(pc.factors.uc[,1], berndt.df[, "MARKET"]))
chart.TimeSeries(cbind(-pc.factors.uc[,1,drop=F], berndt.df[, "MARKET",drop=F]),
                 legend.loc="bottomleft")
cor(cbind(pc.factors.uc[,1,drop=F], berndt.df[, "MARKET",drop=F]))
cor(cbind(-pc.factors.uc[,1,drop=F], berndt.df[, "MARKET",drop=F]))

# use first eigen-vector to compue single factor (with normalization to have pos correlation with market)
# note: cannot treat pc as a portfolio b/c weights do not sum to unity
p1 = pc.fit$loadings[, 1]
p1
sum(p1)
# create factor mimicking portfolio by normalizing weights to unity
p1 = p1/sum(p1)
p1
barplot(p1, horiz=F, main="Factor mimicking weights", col="blue", cex.names = 0.75, las=2)
# create first factor
f1 = returns.mat %*% p1
chart.TimeSeries(f1, main="First principal component factor", colorset="blue")

# estimate factor betas by multivariate regression
X.mat = cbind(rep(1,n.obs), f1)
colnames(X.mat) = c("intercept", "Factor 1")
XX.mat = crossprod(X.mat)
# multivariate least squares
G.hat = solve(XX.mat)%*%crossprod(X.mat,returns.mat)
# can also use solve(qr(X.mat), returns.mat)
beta.hat = G.hat[2,]
E.hat = returns.mat - X.mat%*%G.hat
diagD.hat = diag(crossprod(E.hat)/(n.obs-2))
# compute R2 values from multivariate regression
sumSquares = apply(returns.mat, 2, function(x) {sum( (x - mean(x))^2 )})
R.square = 1 - (n.obs-2)*diagD.hat/sumSquares

# print and plot results
cbind(beta.hat, diagD.hat, R.square)

par(mfrow=c(1,2))
barplot(beta.hat, horiz=T, main="Beta values", col="blue", cex.names = 0.75, las=1)
barplot(R.square, horiz=T, main="R-square values", col="blue", cex.names = 0.75, las=1)
par(mfrow=c(1,1))

# compute covariance/correlation matrices with single pc factor
cov.pc1 = as.numeric(var(f1))*beta.hat%*%t(beta.hat) + diag(diagD.hat)
cor.pc1 = cov2cor(cov.pc1)
# plot correlations using plotcorr() from ellipse package
rownames(cor.pc1) = colnames(cor.pc1)
ord <- order(cor.pc1[1,])
ordered.cor.pc1 <- cor.pc1[ord, ord]
plotcorr(ordered.cor.pc1, col=cm.colors(11)[5*ordered.cor.pc1 + 6])

# compute global min variance portfolio
w.gmin.pc1 = solve(cov.pc1)%*%rep(1,nrow(cov.pc1))
w.gmin.pc1 = w.gmin.pc1/sum(w.gmin.pc1)
colnames(w.gmin.pc1) = "principal.components"

par(mfrow=c(2,1))
barplot(t(w.gmin.pc1), horiz=F, main="Principal Component Weights", col="blue", cex.names = 0.75, las=2)
barplot(t(w.gmin.sample), horiz=F, main="Sample Weights", col="blue", cex.names = 0.75, las=2)
par(mfrow=c(1,1))

#
# asymptotic principal components