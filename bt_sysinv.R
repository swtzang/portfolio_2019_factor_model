# Backtesting based on the toolbox of systematic investor ----
# Reference:
# http://jellenvermeir.info/blog/backtesting-a-global-minimum-variance-portfolio-strategy-in-r/
# https://bookdown.org/sstoeckl/Tidy_Portfoliomanagement_in_R/s-2Data.html#ss_21GetData
# https://systematicinvestor.wordpress.com/2011/12/13/backtesting-minimum-variance-portfolios/
#
library(quantmod)
library(tidyquant)
library(dplyr)
library(tidyverse)
library(XLConnect)
library(parallel)
library(foreach)
library(doParallel)
library(RcppParallel)
library(timetk)
#
# Load Systematic Investor Toolbox (SIT) ----
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)
# Check exchange information ----
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

# Keep and select 10 largest symbols from the S&P500 that are also traded on NYSE or NASDAQ----
stocks.selection <- sp500 %>% 
  inner_join(rbind(nyse, nasdaq) %>% 
               select(symbol, last.sale.price, market.cap, ipo.year), by=c("symbol")) %>% # join datasets
  filter(ipo.year<2000 & !is.na(market.cap)) %>% # filter years with ipo<2000 or ipo=NA and available market cap
  arrange(desc(weight)) %>% # sort in descending order
  slice(1:10) # 

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
stocks.prices.daily <- stocks.prices %>% 
                       select(symbol, date, adjusted) %>%
                       ungroup() %>% 
                       spread(key = symbol, value = adjusted) %>% 
                       tk_xts(date_var = date) # convert from tibble into xts 
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
# Loading FF3F data from web ----
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
# Convert long data to wide data ----
stocks.prices.monthly %>% ungroup() %>% 
  slice(1:5) # show first 5 entries
#
stocks.returns %>% arrange(-desc(symbol)) %>% ungroup() %>% slice(1:5) 
#
index.returns %>% ungroup() %>% slice(1:5)  
# convert factors.returns to wide format
factors.returns %>% slice(1:5)
                     
# merge all data 
# Convert factors.returns to wide format----
factors.returns.w <- factors.returns %>% 
                     spread(key = FFvar, value = ffret)
# Convert to wide format
stocks.prices.monthly.w <- stocks.prices.monthly %>% 
                           select(-volume) %>% 
                           spread(symbol, value = adjusted)
#
stocks.returns.w <- stocks.returns %>% spread(symbol, value = month_ret)
stocks.returns.w                    
# Merge all return data ----
all.ret.merge <- stocks.returns.w %>% left_join(index.returns, by = 'date') %>% 
                 left_join(factors.returns.w, by = 'date')
all.ret.merge

# Merge price, return, volume with symbols in the first column ----
stocks.final <- stocks.prices.monthly %>% bind_cols(stocks.returns) %>%
                          left_join(index.returns, by = 'date') %>% 
                          left_join(factors.returns.w, by = c("date" = "date")) %>% 
                          select(-date1, -symbol1) %>% 
                          dplyr::rename(return = month_ret, sp500 = indexret)
stocks.final 
# Calculate MACD and Signal using mutate_fun() ----
stocks.final %>% group_by(symbol) %>%
                 mutate(date=as.Date(date,frac=1)) %>% 
                 tq_mutate(select     = adjusted, 
                           mutate_fun = MACD, 
                           col_rename = c("MACD", "Signal")) %>%
                 select(symbol,date,adjusted,MACD,Signal) 
                 #tail() # show last part of the dataset
#
save(stocks.final, file="stocks.RData")
#

#==========================================================
stock.10ret <- stocks.returns.w %>% 
               mutate(date=as.Date(date,frac=1)) %>% 
               tk_xts(date_var = date) # convert from tibble into xts 

stock.10price <- stocks.prices.monthly.w %>% 
                 mutate(date=as.Date(date,frac=1)) %>% 
                 tk_xts(date_var = date) # convert from tibble into xts 
###########################################################
# Plot the underlying SPDR assets #
##########################################################
nrAssets <- ncol(stock.10price)
par(mfrow=c(4,3))
res <- sapply(1:nrAssets, function(x) plot(time(stock.10price[,x]),
              coredata(stock.10price[,x]), 
              main=names(stock.10price)[x],type="l",
              xlab="Time",ylab="Price"))

#===============================================================
data <- new.env()
data$prices <- stocks.prices.daily
#bt.prep(data, align='remove.na', dates='2000::2018')
#bt.prep(data.weekly, align='remove.na', dates='1990::2011')

#*****************************************************************
# Code Strategies
#****************************************************************** 
prices = data$prices   
n = ncol(prices)

# find week ends
week.ends = endpoints(prices, 'weeks')
week.ends = week.ends[week.ends > 0]		


# Equal Weight 1/N Benchmark
models<-list()
data$weight = prices * NA
data$execution.price = prices
data$weight[week.ends,] = ntop(prices[week.ends,], n)		

capital = 100000
data$weight[] = (capital / prices) * data$weight
models$equal.weight = bt.run(data)

