# https://rviews.rstudio.com/2018/02/08/capm-beta/

library(tidyquant)
library(tidyverse)
library(timetk)
library(tibbletime)
library(broom)
# 
# + SPY (S&P500 fund) weighted 25%
# + EFA (a non-US equities fund) weighted 25%
# + IJS (a small-cap value fund) weighted 20%
# + EEM (an emerging-mkts fund) weighted 20%
# + AGG (a bond fund) weighted 10%

symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

prices <- 
  getSymbols(symbols, src = 'yahoo', 
             from = "2013-01-01",
             to = "2017-12-31",
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)

prices_monthly <- to.monthly(prices, indexAt = "last", OHLC = FALSE)

asset_returns_xts <- na.omit(Return.calculate(prices_monthly, method = "log"))

w <- c(0.25, 0.25, 0.20, 0.20, 0.10)

portfolio_returns_xts_rebalanced_monthly <- 
  Return.portfolio(asset_returns_xts, weights = w, rebalance_on = "months") %>%
  `colnames<-`("returns") 

asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "last", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

portfolio_returns_tq_rebalanced_monthly <- 
  asset_returns_long %>%
  tq_portfolio(assets_col  = asset, 
               returns_col = returns,
               weights     = w,
               col_rename  = "returns",
               rebalance_on = "months")





