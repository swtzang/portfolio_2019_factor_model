library(pacman)
p_load(tidyverse)
retdata <- read_csv('FamaFrench_mon_69_98_3stocks.csv')
glimpse(retdata)
colnames(retdata)[2]<- 'Mkt_RF'# Replace 'Mkt-RF' with 'Mkt_RF'; 
# attach(retdata)
#===================================================================
# Using FF 3 factor model to compute covariance matrix 
#===================================================================
# Method 1: by "lm" function
#============================
N <- dim(retdata)[1]
ones = rep(1,N)
stock.rets <- retdata %>% select(c(2,3,4,6,7,8))/100
fit3 <- lm(formula = cbind(ge, ibm, mobil) ~ Mkt_RF + SMB + HML, data=stock.rets)
sigF3 = as.matrix(var(cbind(stock.rets$Mkt_RF, 
                            stock.rets$SMB, 
                            stock.rets$HML)))
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
#X = as.matrix(cbind(ones, stock.rets$Mkt_RF))
retdata1 = as.matrix(retdata[,c(6,7,8)]/100)
b_hat.3 = solve(t(X.3)%*%(X.3))%*%t(X.3)%*%retdata1
E_hat.3 = retdata1 - X.3%*%b_hat.3
b_hat.3 = as.matrix(b_hat.3[-1,])
diagD_hat.3 = diag(t(E_hat.3)%*%E_hat.3)/(N-4)
cov_3f.3 = t(b_hat.3)*sigF3*b_hat.3 + diag(diagD_hat.3) 
cov_3f.3
cov_3f

#=====================================
# Rolling regression
#=====================================
# https://systematicinvestor.wordpress.com/2012/06/20/factor-attribution/
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
#setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
library(xts)

p_load(tbl2xts, timetk, quantmod)
window.len <- 36
nperiods <- nrow(stock.rets)

stock.rets <- retdata %>% select(date:HML, ge:mobil) %>% 
              mutate(Date = as.yearmon(as.character(date), "%Y%m")) %>% 
              select(Date, Mkt_RF:mobil) %>%              
              tbl_xts(.)/100
dates <- tk_index(stock.rets, timetk_idx = TRUE)
colnames = c('alpha', 'MKT', 'SMB', 'HML', 'R2')
estimate = make.xts(matrix(NA, nr = nperiods, len(colnames)), dates)
colnames(estimate) = colnames
std.error = estimate

# main loop
# i = 36
for( i in window.len:nperiods ) {
  window.index = (i - window.len + 1) : i
  
  fit = summary(lm(ge ~ Mkt_RF + SMB + HML, data = stock.rets[window.index,]))
  estimate[i,] = c(fit$coefficients[,'Estimate'], fit$r.squared)
  std.error[i,] = c(fit$coefficients[,'Std. Error'], NA)
  
  if( i %% 10 == 0) cat(i, '\n')
}

estimate
std.error
#
fit.all = summary(lm(ge ~ Mkt_RF + SMB + HML, data = stock.rets))
estimate.all = c(fit.all$coefficients[,'Estimate'], fit.all$r.squared)
std.error.all = c(fit.all$coefficients[,'Std. Error'], NA)
#
i = 1
est = estimate[,i]
est.std.error = ifna(std.error[,i], 0)

plota(est, 
      ylim = range( c(
        range(est + est.std.error, na.rm=T),
        range(est - est.std.error, na.rm=T)     
      )))

polygon(c(dates,rev(dates)), 
        c(coredata(est + est.std.error), 
          rev(coredata(est - est.std.error))), 
        border=NA, col=col.add.alpha('red',50))


#
#*****************************************************************
# Reports
#****************************************************************** 
layout(matrix(1:10,nc=2,byrow=T))

for(i in 1:5) { 
#-------------------------------------------------------------------------
# Time plot
#-------------------------------------------------------------------------
  est = estimate[,i]
  est.std.error = ifna(std.error[,i], 0)
  
  plota(est, 
        ylim = range( c(
          range(est + est.std.error, na.rm=T),
          range(est - est.std.error, na.rm=T)     
        )))
  
  polygon(c(dates,rev(dates)), 
          c(coredata(est + est.std.error), 
            rev(coredata(est - est.std.error))), 
          border=NA, col=col.add.alpha('red',50))
  
  est = estimate.all[i]
  est.std.error = std.error.all[i]
  
  polygon(c(range(dates),rev(range(dates))), 
          c(rep(est + est.std.error,2),
            rep(est - est.std.error,2)),
          border=NA, col=col.add.alpha('blue',50))
  
  abline(h=0, col='blue', lty='dashed')
  
  abline(h=est, col='blue')
  
  plota.lines(estimate[,i], type='l', col='red')
  
  #-------------------------------------------------------------------------
  # Histogram
  #-------------------------------------------------------------------------
  par(mar = c(4,3,2,1))
  hist(estimate[,i], col='red', border='gray', las=1,
       xlab='', ylab='', main=colnames(estimate)[i])
  abline(v=estimate.all[i], col='blue', lwd=2)
}




#
source('portfolio_noshorts.r')
globalMin.portfolio()
efficient.frontier()
