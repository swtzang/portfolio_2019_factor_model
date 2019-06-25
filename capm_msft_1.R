##########################
#  Homework 
# Single factor model using capm.csv
#########################
rm(list=ls())
library(tidyquant)
#setwd("D:/亞洲大學上課資料/Portfolio management 2017 Spring")
dat <- read_csv("capm.csv") %>% 
       mutate(Date = as.character(Date) %>% as.Date(., "%Y/%m/%d")) %>% 
       filter(Date>=as.Date("1993-11-01") & Date<= as.Date("1998-11-30")) %>% 
       rename(rf = `Close-tbill`, 
              sp500 = `Close-sp500`, 
              msft = `Close-msft`, 
              ge = `Close-ge`, 
              ford = `Close-ford`) %>% 
              # convert risk-free rate into daily returns
              mutate(rf = rf/(100*360)) 
#
glimpse(dat)
tail(dat)
#

ret4 <- dat %>% select(-rf) %>% 
        gather(key = stock, value = price, -Date) %>%
        group_by(stock) %>% 
        tq_transmute( mutate_fun = periodReturn, 
                      period     = "daily", 
                      type       = "arithmetic",
                      col_rename = "daily.returns") %>% 
        ungroup() %>% 
        spread(stock, daily.returns) %>% 
        bind_cols(., rf = dat$rf) %>%
        # subract each returns by risk-free rate
        mutate(ford_rf = ford - rf, 
               ge_rf   = ge - rf, 
               msft_rf = msft - rf, 
               sp500_rf = sp500 - rf) %>%
       # Delete the first row with 0 data
       slice(-1) %>%
       select(Date, ends_with("_rf"))
#
ret4.reg <- ret4 %>% lm(formula = cbind(msft_rf, ge_rf, ford_rf) ~ sp500_rf, data = .)
b_hat <- ret4.reg$coefficients
# compute residual variance and diagonalized it
diagD_hat <- ret4.reg$residuals %>% cov() %>% diag() %>% diag(nrow = length(.))

# covariance matrix by single factor model
Y = ret4$sp500_rf
cov_factor = as.numeric(var(Y))*t(b_hat)%*%b_hat + diagD_hat 
cov_factor
#---------------------------------------------------------------------------
# You can also use OLS formula: beta=inv(X'X)X'Y to get the estimated beta
#---------------------------------------------------------------------------
n = length(Y)
ones = rep(1,n)
X = cbind(ones, Y)
X = as.matrix(X)
Y = cbind(ret4$msft_rf, ret4$ge_rf, ret4$ford_rf)
b_hat.1 = solve(t(X)%*%X)%*%t(X)%*%Y
b_hat.1
b_hat

# follow the formula in the slides
E_hat = Y - X%*%b_hat.1
res_var.1 = diag(t(E_hat)%*%E_hat)/(n-2)
diagD_hat.1 = diag(res_var.1)
diagD_hat.1
# covariance matrix by single factor model

cov_factor.1 = as.numeric(var(EX_R_sp500[-1,]))*t(b_hat.1)%*%b_hat.1 + diagD_hat.1 
cov_factor.1 
cov_factor
