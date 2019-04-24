##########################
#  Homework 
# Single factor model using capm.csv
#########################
rm(list=ls())
#setwd("D:/亞洲大學上課資料/Portfolio management 2017 Spring")
dat = read.csv("capm.csv",header=T)

# Convert data into time series
library(xts)
dat.xts<-xts(dat[,2:6], order.by= as.Date(dat[,1], "%Y/%m/%d"))
# Sample period
dat.xts.sample<-dat.xts['199311/199811']

# Calculate returns for three stocks and minus daily treasury bill rate
# Daily treasury bill rate can be obtained by converting annual rate into daily rates
riskfree<-dat.xts.sample[,1]/(100*252)
EX_R_sp500 = dat.xts.sample[,2]/lag(dat.xts.sample[,2],1) - 1 - riskfree   
EX_R_msft = dat.xts.sample[,3]/lag(dat.xts.sample[,3],1) - 1 - riskfree  
EX_R_ge = dat.xts.sample[,4]/lag(dat.xts.sample[,4],1) - 1 - riskfree  
EX_R_ford = dat.xts.sample[,5]/lag(dat.xts.sample[,5],1) - 1 - riskfree  
# Delete the first row NA data
fit1 = lm(EX_R_msft[-1,]~EX_R_sp500[-1,])
fit2 = lm(EX_R_ge[-1,]~EX_R_sp500[-1,])
fit3 = lm(EX_R_ford[-1,]~EX_R_sp500[-1,])
#options(digits=3)
summary(fit1)
summary(fit2)
summary(fit3)
# Extract beta for fit1
beta_hat<-rbind(fit1$coefficients, fit2$coefficients, fit3$coefficients)
b_hat<-t(beta_hat)
#degree of freedom
n<-dim(EX_R_msft[-1,])[1]
df = n-2
#residual variance
res_var  = c(sum((fit1$residuals)^2), sum((fit2$residuals)^2), sum((fit3$residuals)^2))/df
diagD_hat = diag(res_var)
diagD_hat
# covariance matrix by single factor model
Y = cbind(EX_R_msft[-1,], EX_R_ge[-1,], EX_R_ford[-1,])
cov_factor = var(Y)*t(b_hat)%*%b_hat + diag(diagD_hat) 
cov_factor
#---------------------------------------------------------------------------
# You can also use OLS formula: beta=inv(X'X)X'Y to get the estimated beta
#---------------------------------------------------------------------------
ones = rep(1,n)
X = cbind(ones, EX_R_sp500)
X = as.matrix(X[-1,])
Y = cbind(EX_R_msft, EX_R_ge, EX_R_ford)
Y = as.matrix(Y[-1,])
b_hat.1 = solve(t(X)%*%X)%*%t(X)%*%Y
b_hat.1
b_hat

# follow the formula in the slides
E_hat = Y - X%*%b_hat.1
res_var.1 = diag(t(E_hat)%*%E_hat)/((n-1)-2)
diagD_hat.1 = diag(res_var.1)
diagD_hat.1
# covariance matrix by single factor model
cov_factor.1 = var(Y)*t(b_hat)%*%b_hat + diag(diagD_hat) 
cov_factor.1 
cov_factor
