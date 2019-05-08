#========================================
# FamaFrench_mon_69_98_3stocks
# one factor model
# ff three factor model
#========================================
library(tidyverse)
retdata = read_csv('FamaFrench_mon_69_98_3stocks.csv')
glimpse(retdata)
colnames(retdata)[2]<- 'Mkt_RF'
# attach(retdata)
#Below we use two different approaches to estimate covariance matrix 
#===========================================================
# Single index model to compute covariance matrix
#===========================================================
# Method 1: by "lm" function
#===========================
stock.rets<-retdata %>% select(c(2,6,7,8))/100
glimpse(stock.rets)
N <- dim(stock.rets)[1]
#Mkt.RF<-retdata %>% select(2)/100
fit = lm(formula = cbind(ge,ibm,mobil)~Mkt_RF, data = stock.rets)
sigF = as.numeric(var(Mkt.RF))
bbeta = as.matrix(fit$coefficients)
bbeta = as.matrix(bbeta[-1,])
bbeta
sigeps = crossprod(fit$residuals)/(N-2)
# sigeps = as.matrix(var(fit$residuals)) #  you can use this way too
sigeps = diag(diag(sigeps))
sigeps
cov_1f = sigF*bbeta%*%t(bbeta)+sigeps
cov_1f
#===================================
#Method 2: by formula "inv(X'X)*X'Y"
#===================================
ones = rep(1,N)
X = as.matrix(cbind(ones, stock.rets$Mkt_RF))
retdata1 = as.matrix(retdata[,c(6,7,8)]/100)
b_hat = solve(t(X)%*%X)%*%t(X)%*%retdata1
E_hat = retdata1 - X%*%b_hat
b_hat = as.matrix(b_hat[-1,])
diagD_hat = diag(t(E_hat)%*%E_hat)/(N-2)
cov_1f.1 = as.numeric(var(Mkt.RF))*b_hat%*%t(b_hat) + diag(diagD_hat); 
cov_1f.1
#===================================================================
# Using FF 3 factor model to compute covariance matrix 
#===================================================================
# Method 1: by "lm" function
#============================
stock.rets<-retdata %>% select(c(2,3,4,6,7,8))/100
fit3 = lm(formula = cbind(ge, ibm, mobil)~Mkt_RF + SMB + HML, data=stock.rets)
sigF3 = as.matrix(var(cbind(Mkt.RF, SMB, HML)))
bbeta3 = as.matrix(fit3$coefficients)
bbeta3 = bbeta3[-1,]
bbeta3
sigeps3 = crossprod(fit3$residuals)/(N-4)
sigeps3 = diag(diag(sigeps3))
cov_3f = t(bbeta3) * sigF3 * (bbeta3) + sigeps3
cov_3f
#===================================
#Method 2: by formula "inv(X'X)*X'Y"
#===================================
X.3 = cbind(ones, stock.rets$Mkt_RF, stock.rets$SMB, stock.rets$HML)
b_hat.3 = solve(t(X.3)%*%(X.3))%*%t(X.3)%*%retdata1
E_hat.3 = retdata1 - X.3%*%b_hat.3
b_hat.3 = as.matrix(b_hat.3[-1,])
diagD_hat.3 = diag(t(E_hat.3)%*%E_hat.3)/(N-4)
cov_3f.3 = t(b_hat.3)*sigF3*b_hat.3 + diag(diagD_hat.3) 
cov_3f.3


#======================================================
# Create frontier function to plot efficient frontier
#======================================================
frontier <- function(return, Q) {
  #return <- log(tail(assets, -1) / head(assets, -1))
  n = ncol(return)
  #Q = cov(return)
  Ax <- rbind(2*cov(return), colMeans(return), rep(1, n))
  Ax <- cbind(Ax, rbind(t(tail(Ax, 2)), matrix(0, 2, 2)))
  r <- colMeans(return)
  rbase <- seq(min(r), max(r), length = 100)
  s <- sapply(rbase, function(x) {
    b0 <- c(rep(0, ncol(return)), x, 1)
    y <- head(solve(Ax, b0), n)
    sqrt(y%*%Q%*%y)
  })
  efficient.port <- list("call" = call,
                         "er" = as.vector(rbase),
                         "sd" = as.vector(s))
  class(efficient.port) <- "portfolio"
  efficient.port
  #plot(s, rbase, xlab = 'Std', ylab = 'Return', type="l")
}




#===============================================================
# Use different covariance matrix to plot efficient frontier: Q
# 
#================================================================
#return = retdata1
Q.3f = cov_3f
Q.1f = cov_1f
retdata1
Q = cov(retdata1)

#========================================
# draw overlay frontiers on the same graph
#=========================================
xy.3f = frontier(retdata1, Q.3f)
xy.1f = frontier(retdata1, Q.1f)
xy    = frontier(retdata1, Q)
#
#xx<-c(xy$sd, xy.1f$sd, xy.3f$sd)
#yy<-c(xy$er, xy.1f$er, xy.3f$er)
#type<-rep(c("hist", "1.factor", "3.factor"), c(100,100,100))
#xy.all<-data.frame(xx, yy, type)
#head(xy.all)

#
#library(lattice)
#xyplot(yy ~ xx, xy.all, groups = xy.all$type, pch= 20)
#ggplot(xy.all, aes(x = xx, y = yy, colour = type))+
#  geom_line(type)

#plot(xx, yy)

plot(xy$sd, xy$er, type = 'l', col="red")
lines(xy.1f$sd, xy.1f$er, col = "green")
lines(xy.3f$sd, xy.3f$er, col = "red")
#==========================================================================
# Another way to draw overlay frontiers on the same graph
# Ref: http://www.sixhat.net/plotting-multiple-data-series-in-r.html
#==========================================================================
plot(xy.3f$sd, xy.3f$er, type="l",col="red", xlab="risk", ylab="return")
par(new=TRUE)
plot(xy.1f$sd, xy.1f$er, axes=F, type="l",col="blue", xlab="", ylab="")
par(new=TRUE)
plot(xy$sd, xy$er, axes=F, type="l",col="black",  xlab="", ylab="")
#text(locator(), labels = c("red line", "black line)"))

