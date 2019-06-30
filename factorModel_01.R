# econ589factorModels.r
# Examples for factor model risk lecture
# author: Eric Zivot
# created: January 10, 2011
# updated: May 22, 2012
#
# comments: Examples follow chapter 11 in Zivot and Wang (2006), Modeling Financial
# Time Series with S-PLUS.

# set output options
rm(list=ls())
options(width = 70, digits=4)

# load required packages
library(ellipse)
#install.packages("fEcofin", repos="http://R-Forge.R-project.org")
install.packages("fEcofin", repos="http://R-Forge.R-project.org")
library(fEcofin)                # various data sets
library(PerformanceAnalytics)   # performance and risk analysis functions
library(zoo)
library(tidyquant)
library(psych)

################################################################################
# Macroeconomic Factor Models
################################################################################

##
## Single Index Model
##

# load Berndt Data
data(berndtInvest)
str(berndtInvest)

# create data frame with dates as rownames
berndt.df = berndtInvest[, -1]
rownames(berndt.df) = as.character(berndtInvest[, 1])
colnames(berndt.df)

##
## use multivariate regression and matrix algebra
##

returns.mat = as.matrix(berndt.df[, c(-10, -17)])
# check distribution and outliers
violinBy(returns.mat)
#
market.mat = as.matrix(berndt.df[,10, drop=F])
n.obs = nrow(returns.mat)
X.mat = cbind(rep(1,n.obs),market.mat)
colnames(X.mat)[1] = "intercept"
XX.mat = crossprod(X.mat)

# multivariate least squares
G.hat = solve(XX.mat)%*%crossprod(X.mat,returns.mat)
# can also use solve(qr(X.mat), returns.mat)
beta.hat = G.hat[2,]
beta.hat
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

# compute single index model covariance/correlation matrices
cov.si = as.numeric(var(market.mat))*beta.hat%*%t(beta.hat) + diag(diagD.hat)
cor.si = cov2cor(cov.si)
# plot correlations using plotcorr() from ellipse package
rownames(cor.si) = colnames(cor.si)
ord <- order(cor.si[1,])
ordered.cor.si <- cor.si[ord, ord]
plotcorr(ordered.cor.si, col=cm.colors(11)[5*ordered.cor.si + 6])
# compare to sample correlation matrix
cor.sample = cor(returns.mat)
ord <- order(cor.sample[1,])
ordered.cor.sample <- cor.sample[ord, ord]
plotcorr(ordered.cor.sample, col=cm.colors(11)[5*ordered.cor.sample + 6])

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
# barplot weights side by side
cbind(w.gmin.si, w.gmin.sample) %>% 
  as_tibble(rownames = "stock") %>% 
  gather(model, weight, -stock ) %>% 
  ggplot(aes(stock, weight, fill = model)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw() +
  labs(title = "Optimal weights of global minimum variance portfolio",
       subtitle = "Covariance from sample and single index model" )
#

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
for (i in asset.names) {
  reg.df = berndt.df[, c(i, "MARKET")]
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
dataToPlot = cbind(fitted(reg.list$CITCRP), berndt.df$CITCRP)
colnames(dataToPlot) = c("Fitted","Actual")
dataToPlot.xts<-as.xts(dataToPlot, order.by = as.Date(rownames(dataToPlot)))
chart.TimeSeries(dataToPlot.xts, main="Single Index Model for CITCRP",
                 colorset=c("black","blue"), legend.loc="bottomleft")

# scatterplot of the single index model regression
plot(berndt.df$MARKET, berndt.df$CITCRP, main="SI model for CITCRP",
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
# multivariate OLS regression to estimate K x T matrix of factor returns  (K=3)
F.hat = solve(crossprod(B.mat))%*%t(B.mat)%*%returns.mat
# rows of F.hat are time series of estimated industry factors
t(F.hat)

# plot industry factors in separate panels - convert to zoo objects for plotting
F.hat.zoo = zoo(t(F.hat), as.Date(colnames(F.hat)))
head(F.hat.zoo)

# panel function to put horizontal lines at zero in each panel
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
plot(F.hat.zoo, main="OLS estimates of industry factors",
     panel=my.panel, lwd=2, col="blue")


# compute N x T matrix of industry factor model residuals
E.hat = returns.mat - B.mat%*%F.hat
# compute residual variances from time series of errors
diagD.hat = apply(E.hat, 1, var)
Dinv.hat = diag(diagD.hat^(-1))
# multivariate FGLS regression to estimate K x T matrix of factor returns
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

# Barplot the weights side by side
colnames(w.gmin.ind) <- "FM"
cbind(w.gmin.ind, w.gmin.sample) %>% 
  as_tibble(rownames = "stock") %>% 
  gather(model, weight, -stock ) %>% 
  ggplot(aes(stock, weight, fill = model)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw() +
  labs(title = "Optimal weights of global minimum variance portfolio",
       subtitle = "Covariance from industry factor model and sample returns" )

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
# Principal Component Analysis ----
# https://systematicedge.wordpress.com/2013/06/02/principal-component-analysis-in-portfolio-management/

## Method 1: Use prcomp() function for principal component analysis ----
demean = scale(coredata(returns.mat), center=TRUE, scale=FALSE)
covm<-cov(demean)
evec<-eigen(cov(demean), symmetric=TRUE)$vector[] #eigen vectors
eval<-eigen(cov(demean), symmetric=TRUE)$values #eigen values
eval
#PCA Functional
pca<-prcomp(returns.mat)
evec<-pca$rotation[] #eigen vectors
eval <- pca$sdev^2 #eigen values
#
inv.evec<-solve(evec) #inverse of eigenvector
pc.port<-inv.evec %*% t(returns.mat)
pc.port.fc <- pc.port %>% t() %>% as.data.frame()
# But the result is different from those from princomp()
# cbind(pc.port.fc[,1], pc.fit$scores[,1], pc.factors.uc[,1])
#
# # Method 2: Use principal() function for principal component analysis ----
pcapsych = principal(returns.mat, rotate='varimax')
print(pcapsych)

# Method 3: Use factanal() function for principal component analysis ----
d.fa <- factanal(returns.mat, factors = 3, rotation="varimax")
print(d.fa,digits =2, cutoff =.2, sort =TRUE)

# Method 4: Use princomp() function for principal component analysis ----
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
pc.fit$scores[, 1, drop=FALSE] %>% 
  xts(order.by=as.Date(rownames(.), "%Y-%m-%d")) %>% 
  chart.TimeSeries(colorset="blue")

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
cbind(pc.fit$scores[,1,drop=F], -pc.factors.uc[,1,drop=F]) %>% 
  xts(order.by=as.Date(rownames(.), "%Y-%m-%d")) %>% 
  chart.TimeSeries(
    main="Centered and Uncentered Principle Component Factors",
    legend.loc="bottomleft")

# compare first pc factor with market return
cbind(pc.factors.uc[,1, drop=F], berndt.df[, "MARKET", drop=F]) %>% 
  xts(order.by=as.Date(rownames(.), "%Y-%m-%d")) %>% 
  chart.TimeSeries(legend.loc = "bottomleft")

cbind(-pc.factors.uc[,1,drop=F], berndt.df[, "MARKET",drop=F]) %>% 
  xts(order.by=as.Date(rownames(.), "%Y-%m-%d")) %>% 
  chart.TimeSeries(legend.loc="bottomleft")

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
#
f1 %>% xts(order.by=as.Date(rownames(.), "%Y-%m-%d")) %>%  
  chart.TimeSeries(main="First principal component factor", colorset="blue")

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
#
cbind(w.gmin.pc1, w.gmin.sample) %>% 
  as_tibble(rownames = "stock") %>% 
  gather(model, weight, -stock ) %>% 
  ggplot(aes(stock, weight, fill = model)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw() +
  labs(title = "Optimal weights of global minimum variance portfolio",
       subtitle = "Covariance from PC1 and sample returns", 
       caption  = "1st PC factor")

#
par(mfrow=c(2,1))
barplot(t(w.gmin.pc1), horiz=F, main="Principal Component Weights", col="blue", cex.names = 0.75, las=2)
barplot(t(w.gmin.sample), horiz=F, main="Sample Weights", col="blue", cex.names = 0.75, las=2)
par(mfrow=c(1,1))

#
# asymptotic principal components
#