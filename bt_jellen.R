#===========================
# Reference:
# http://jellenvermeir.info/blog/backtesting-a-global-minimum-variance-portfolio-strategy-in-r/
# https://bookdown.org/sstoeckl/Tidy_Portfoliomanagement_in_R/s-2Data.html#ss_21GetData
#===========================
library(quantmod)
library(tidyquant)
library(dplyr)
library(tidyverse)
library(XLConnect)
#
tq_exchange_options() # find all exchanges available
#
tq_index_options() # find all indices available
#
tq_get_options() # find all data sources available
#
sp500 <- tq_index("sp500")
glimpse(sp500)
# 
nyse <- tq_exchange('nyse')
glimpse(nyse)
#
nasdaq <- tq_exchange('nasdaq')
glimpse(nasdaq)

# We only want to keep symbols from the S&P500 that are also traded on NYSE or NASDAQ
stocks.selection <- sp500 %>% 
  inner_join(rbind(nyse, nasdaq) %>% 
               select(symbol, last.sale.price, market.cap, ipo.year), by=c("symbol")) %>% # join datasets
  filter(ipo.year<2000 & !is.na(market.cap)) %>% # filter years with ipo<2000 or ipo=NA and available market cap
  arrange(desc(weight)) %>% # sort in descending order
  slice(1:10)

# We will download stock prices from yahoo. Data from that source usually comes in the OHLC 
# format (open,high,low,close) with additional information (volume, adjusted).
stocks.prices <- stocks.selection$symbol %>% 
  tq_get(get  = "stock.prices",from = "2000-01-01",to = "2018-12-31") %>%
  group_by(symbol)
#
index.prices <- "^GSPC" %>% 
  tq_get(get  = "stock.prices", from = "2000-01-01",to = "2018-12-31") 

# show the first two entries of each group
stocks.prices %>% slice(1:2) 

# dividends and stock splits can also be downloaded
stocks.dividends <- stocks.selection$symbol %>% 
  tq_get(get  = "dividends",from = "2000-01-01",to = "2018-12-31") %>%
  group_by(symbol)
stocks.dividends

stocks.splits <- stocks.selection$symbol %>% 
  tq_get(get  = "splits",from = "2000-01-01",to = "2018-12-31") %>%
  group_by(symbol)
stocks.splits

# We show how to change the periodicity of the data (where we keep the adjusted close price and 
# the volume information) and calculate monthly log returns for the ten stocks and the index. 
# We then merge the price and return information for each stock, and at each point in time 
# add the return of the S&P500 index and the 3 Fama-French-Factors.

stocks.prices.monthly <- stocks.prices %>% 
  tq_transmute(select = c(adjusted,volume), # which column to choose
               mutate_fun = to.monthly,     # funtion: make monthly series
               indexAt = "lastof") %>%      # ‘yearmon’, ‘yearqtr’, ‘firstof’, ‘lastof’, ‘startof’, or ‘endof’
  ungroup() %>% mutate(date=as.yearmon(date))
#
stocks.prices.monthly
#
stocks.returns <- stocks.prices %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,   # create monthly  returns
               period="monthly", 
               type="arithmetic", 
               col_rename = 'month_ret'
  ) %>% 
  #mutate(mret = scales::percent(mret)) %>% 
  ungroup() %>% mutate(date=as.yearmon(date)) 
#
stocks.returns
#
index.returns <- index.prices %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn, 
               period="monthly", 
               type="arithmetic", 
               col_rename = 'indexret') %>% 
  mutate(date=as.yearmon(date))
#
index.returns
#--------------------------------------------------
# Fama-French Data (Kenneth French’s Data Library)
install.packages("remotes")
remotes::install_github("sstoeckl/ffdownload")
#devtools::install_bitbucket("sstoeckl/FFdownload")
#devtools::install_github("sstoeckl/ffdownload", force = TRUE)
#
library(FFdownload)
FFdownload(output_file = "FFdata.RData", # output file for the final dataset
           tempdir = NULL, # where should the temporary downloads go to (create automatically)
           exclude_daily = TRUE, # exclude daily data
           download = FALSE) # if false, data already in the temp-directory will be used
#
load(file = "FFdata.RData")
#FFdownload <- source('ffdata.rds')
# Extract FF3F monthly returns
library(timetk)
factors <- FFdownload$`x_F-F_Research_Data_Factors`$monthly$Temp2 %>% 
  tk_tbl(rename_index="date") %>% # make tibble
  # frac=1 which means the end of the month 
  # (frac=0 would mean the beginning of the month).
  mutate(date=as.Date(date, frac=1)) %>% # make proper month-end date format
  gather(key=FFvar, value = price, -date) # gather into tidy format

factors %>% group_by(FFvar) %>% slice(1:2) %>% ungroup()

#
factors.returns <- factors %>% 
  mutate(ffret=price/100) %>%  # already is monthly
  select(-price) %>% 
  mutate(date=as.yearmon(date))
  
factors.returns
#
stocks.prices.monthly %>% ungroup() %>% 
  slice(1:5) # show first 5 entries
#
stocks.returns %>% arrange(-desc(symbol)) %>% ungroup() %>% slice(1:5) 
#
index.returns %>% ungroup() %>% slice(1:5)  
# convert factors.returns to wide format----
factors.returns %>% slice(1:5)
                     
#stocks.ratios <- stocks.selection$symbol %>% 
#                 tq_get(get  = "key.ratios",from = "2000-01-01",to = "2017-12-31") %>%
#                 group_by(symbol)
# merge all data 

# convert factors.returns to wide format----
factors.returns.w <- factors.returns %>% 
                     spread(key = FFvar, value = ffret)
# convert to wide format
stocks.returns.w <- stocks.returns %>% spread(symbol, value = month_ret)
                    
#
all.ret.merge <- stocks.returns.w %>% left_join(index.returns, by = 'date') %>% 
                 left_join(factors.returns.w, by = 'date')
all.ret.merge
# merge price, return, volume with symbols in the first column
all.merge <- stocks.prices.monthly %>% bind_cols(stocks.returns) %>%
                          left_join(index.returns, by = 'date') %>% 
                          left_join(factors.returns.w, by = c("date" = "date")) %>% 
                          select(-date1, -symbol1) %>% 
                          dplyr::rename(return = month_ret, sp500 = indexret)
all.merge 


#==========================================================
tickers = c('AMZN', 'GOOG', 'MSFT', 'JNJ', 'JPM')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

data %>% ls() 
data$AMZN


SPDR.returns <- na.omit((SPDR / lag(SPDR, k= 1) - 1) * 100)

###########################################################################
############# Plot the underlying SPDR assets #############################
###########################################################################
nrAssets <- ncol(SPDR)
par(mfrow=c(rep(ceiling(sqrt(nrAssets)),2)))
res <- sapply(1:nrAssets, function(x) plot(time(SPDR[,x]),
                                           coredata(SPDR[,x]), main=names(SPDR)[x],type="l",
                                           xlab="Time",ylab="Price"))


## gmv optimization
## Function uses return timeseries xts-object as input
GMVOptimization <- function(returns, covariance.f="CovClassic", longOnly=FALSE, max.weight=1)
{
  estimation <- do.call(covariance.f, 
                        list(x=coredata(returns)))
  covariance.matrix <- rrcov::getCov(estimation)
  
  # The quadratic solver below implements the dual method of Goldfarb and Idnani (1982, 1983) 
  # for solving quadratic programming problems of the form min(-d^T b + 1/2 b^T D b) 
  # with the constraints A^T b >= b_0.
  #
  # Here, we mimize b^T D b, with D the covariance matrix and b the target weights
  # Constraints are full investment of capital and/or long only
  nrMarginals <- ncol(covariance.matrix)
  # no linear components in target function
  dvec <- rep.int(0, nrMarginals)
  
  # Fully Invested (equality constraint)
  a <- rep.int(1, nrMarginals)
  b <- 1
  
  if(longOnly)
  {
    # Long only (>= constraint)
    a2 <- diag(nrMarginals)
    b2 <- rep.int(0, nrMarginals)
    
    a <- rbind(a, a2)
    b <- c(b, b2)
  }
  
  # Weights greater than -max.weight
  a3 <- diag(nrMarginals)
  b3 <- rep.int(-max.weight, nrMarginals)
  
  # Weights smaller than max.weight
  a4 <- -diag(nrMarginals)
  b4 <- rep.int(-max.weight, nrMarginals)
  
  Amat <- t(rbind(a, a3, a4)) # This matrix will be transposed again
  bvec <- c(b, b3, b4)
  
  # Solve the quadratic problem
  gmv <- solve.QP(Dmat=covariance.matrix, dvec=dvec, Amat=Amat,
                  bvec=bvec, meq=1) # meq = 1 equality constraint
  return(gmv$solution)
}