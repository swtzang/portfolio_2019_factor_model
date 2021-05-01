#==================================================================================================
# Multiple Factor Model – Building CSFB Factors
# https://systematicinvestor.wordpress.com/2012/01/29/multiple-factor-model-fundamental-data/
#==================================================================================================


###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)


###############################################################################
# determine date when fundamental data is available
# use 'date preliminary data loaded' when available
# otherwise lag 'quarter end date' 2 months for Q1/2/3 and 3 months for Q4
###############################################################################     
date.fund.data <- function(data)
{
  # construct date
  quarter.end.date = as.Date(paste(data['quarter end date',], '/1', sep=''), '%Y/%m/%d')  
  quarterly.indicator = data['quarterly indicator',]
  date.preliminary.data.loaded = as.Date(data['date preliminary data loaded',], '%Y-%m-%d') + 1
  
  months = seq(quarter.end.date[1], tail(quarter.end.date,1)+365, by='1 month') 
  index = match(quarter.end.date, months)
  quarter.end.date = months[ iif(quarterly.indicator == '4', index+3, index+2) + 1 ] - 1
  
  fund.date = date.preliminary.data.loaded
  fund.date[is.na(fund.date)] = quarter.end.date[is.na(fund.date)] 
  
  return(fund.date)
}

#*****************************************************************
# Load historical fundamental data
# http://advfn.com/p.php?pid=financials&symbol=NYSE:WMT&mode=quarterly_reports
#****************************************************************** 
Symbol = 'NYSE:WMT'
fund = fund.data(Symbol, 80)

# save fundamental data of quarterly financial report of Walmart
# save(fund, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.fund.Rdata')

# construct date
fund.date = date.fund.data(fund)    

#*****************************************************************
# Create and Plot Earnings per share
#****************************************************************** 
EPS.Q = as.double(fund['Diluted EPS from Total Operations',])
EPS.Q = as.xts(EPS.Q, fund.date)    
EPS = runSum(EPS.Q, 4)

# Plot
layout(1:2)
par(mar=c(2,2,2,1))
x = barplot(EPS.Q, main='Wal-Mart Quarterly Earnings per share', border=NA)
text(x, EPS.Q, fund['quarterly indicator',], adj=c(0.5,-0.3), cex=0.8, xpd = TRUE)

barplot(EPS, main='Wal-Mart Rolling Annual Earnings per share', border=NA)

#You can see a pronounced seasonality in the Quarterly EPS for Wal-Mart, the Q4 always strong and stands out. 
#The common way to deal with seasonality in the income statement is to use rolling annual sum, i.e. sum last 4 quarters.
#Next let’s align Wal-Mart prices and EPS and plot them on the same graph.

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')
tickers = 'WMT'

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

data$WMT = merge(data$WMT, EPS)
# back fill EPS
data$WMT$EPS = ifna.prev(coredata(data$WMT$EPS))    

# Plot
y = data$WMT['1990::']
plota(Cl(y), type = 'l', LeftMargin=3)

plota2Y(y$EPS, type='l', las=1, col='red', col.axis = 'red')

plota.legend('WMT(rhs),WMT.EPS(lhs)', 'blue,red', list(Cl(y),y$EPS))

#Next let’s repeat the above steps for all companies in the Dow Jones index.
#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')       
tickers = dow.jones.components()

# get fundamental data
data.fund <- new.env()
temp = paste(iif( nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep='')
for(i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 80)
save(data.fund, file='data.fund.Rdata')


# get pricing data
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)    
save(data, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.Rdata')


#load(file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.fund.Rdata')
#load(file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.Rdata')


# combine fundamental and pricing data
for(i in tickers) {
  fund = data.fund[[i]]
  fund.date = date.fund.data(fund)
  
  EPS.Q = as.double(fund['Diluted EPS from Total Operations',])
  EPS.Q = as.xts(EPS.Q, fund.date)    
  EPS = runSum(EPS.Q, 4)
  
  data[[i]] = merge(data[[i]], EPS)
}

bt.prep(data, align='keep.all', dates='1995::2014')

#It takes a while to download historical fundamental data for all companies in the Dow Jones index, 
#so I recommend saving your results with save(data.fund, file=’data.fund.Rdata’) command. 
#Later on if you want to run code one more time, just load(file=’data.fund.Rdata’) instead of downloading all data again.

#Next let’s create monthly factors. 
#EP factor = (Earnings per share) / Price. 
#VOMO factor = Volume x Momentum.

#*****************************************************************
# Compute monthly factors
#****************************************************************** 
prices = data$prices
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))

# create factors
factors = list()

# E/P
EPS = bt.apply(data, function(x) ifna.prev(x[, 'EPS']))
factors$EP = EPS / prices

# VOMO - Volume x Momentum
Vo(data)
volume = bt.apply(data, function(x) ifna.prev(Vo(x)))
factors$VOMO = (prices / mlag(prices,10) - 1) * bt.apply.matrix(volume, runMean, 22) / bt.apply.matrix(volume, runMean, 66)


# find month ends
month.ends = endpoints(prices, 'months')

prices = prices[month.ends,]
n = ncol(prices)
nperiods = nrow(prices)

ret = prices / mlag(prices) - 1
next.month.ret = mlag(ret, -1)

factors$EP = factors$EP[month.ends,]    
factors$VOMO = factors$VOMO[month.ends,]   

#Next let’s run correlation analysis for EP factor. 
#You can do correlation analysis for VOMO factor as a homework.
#*****************************************************************
# Correlation Analysis
#****************************************************************** 
x = as.vector(factors$EP)
y = as.vector(next.month.ret)

cor.test(x, y, use = 'complete.obs', method = 'pearson')            

# Plot
par(mar=c(4,4,2,1))             
plot(x, y, pch=20, main='Correlation Analysis for EP factor', xlab='EP', ylab='Next Month Return')
abline(lm(y ~ x), col='blue', lwd=2)

cor.test(x, y, use = 'complete.obs', method = 'pearson')
#####

x1 = as.vector(factors$VOMO)
cor.test(x1, y, use = 'complete.obs', method = 'pearson') 

###########################################################
# 2nd part of MFM
# https://systematicinvestor.wordpress.com/2012/02/04/multiple-factor-model-building-fundamental-factors/
##########################################################

#*****************************************************************
# Find Sectors for each company in DOW 30
#****************************************************************** 
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
tickers.desc = spl('ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities')

sector.map = c()
for(i in 1:len(tickers)) {
  sector.map = rbind(sector.map, 
                     cbind(sector.spdr.components(tickers[i]), tickers.desc[i])
  )
}
colnames(sector.map) = spl('ticker,sector')

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')       
tickers = dow.jones.components()

sectors = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
names(sectors) = tickers

# get fundamental data
load(file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.fund.Rdata')

# get pricing data
load(file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.Rdata')

#*****************************************************************
# Combine fundamental and pricing data
#******************************************************************     
for(i in tickers) {
  fund = data.fund[[i]]
  fund.date = date.fund.data(fund)
  
  # Earnings per Share        
  EPS = get.fund.data('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling=T)
  
  # Sales, exception not available for financial firms
  SALE = get.fund.data('total revenue', fund, fund.date, is.12m.rolling=T)
  
  # Common Shares Outstanding
  CSHO = get.fund.data('total common shares out', fund, fund.date)
  
  # Common Equity
  CEQ = get.fund.data('total equity', fund, fund.date)
  
  # Dividends
  DV.PS = get.fund.data('dividends paid per share', fund, fund.date, is.12m.rolling=T)
  
  # Cash Flow, exception not available for financial firms
  CFL = get.fund.data('net cash from operating activities', fund, fund.date, cash.flow=T, is.12m.rolling=T)
  
  # merge
  data[[i]] = merge(data[[i]], EPS, SALE, CSHO, CEQ, DV.PS, CFL)
}

bt.prep(data, align='keep.all', dates='1995::2014')

#*****************************************************************
# Create Factors
#****************************************************************** 
prices = data$prices
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))

sectors = sectors[colnames(prices)] 

# create factors
factors = list()    

#In the Dow Jones index there are 4 financial firms (AXP, BAC, JPM, TRV) and Sales and Cash Flow are not 
#really measurable for financial firms. Please read Valuing Financial Service Firms by A. Damodaran for 
#detailed explanation why Sales and Cash Flow are not really measurable for financial firms.

#Next let’s create Traditional Value factors: Price/Earnings, Price/Sales, Price/Cash Flow, Dividend Yield, Price/Book.

#*****************************************************************
# Traditional Value
#****************************************************************** 
factors$TV = list()

# Market Value - capitalization
CSHO =  bt.apply(data, function(x) ifna.prev(x[, 'CSHO']))
MKVAL = prices * CSHO

# Price / Earnings
EPS = bt.apply(data, function(x) ifna.prev(x[, 'EPS']))
factors$TV$EP = EPS / prices

# Price / Trailing Sales
SALE = bt.apply(data, function(x) ifna.prev(x[, 'SALE']))   
factors$TV$SP = SALE / MKVAL

# Price / Trailing Cash Flow
CFL = bt.apply(data, function(x) ifna.prev(x[, 'CFL']))
factors$TV$CFP = CFL / MKVAL

# Dividend Yield
DV.PS = bt.apply(data, function(x) ifna.prev(x[, 'DV.PS']))
factors$TV$DY = DV.PS / prices

# Price / Book Value        
CEQ = bt.apply(data, function(x) ifna.prev(x[, 'CEQ']))
factors$TV$BP = CEQ / MKVAL

# Eliminate Price/Sales and Price/Cash Flow for financial firms
factors$TV$SP[, sectors == 'Financials'] = NA
factors$TV$CFP[, sectors == 'Financials'] = NA

#*****************************************************************
# Convert to monthly
#****************************************************************** 
# find month ends
month.ends = endpoints(prices, 'months')

prices = prices[month.ends,]
n = ncol(prices)
nperiods = nrow(prices)

ret = prices / mlag(prices) - 1
next.month.ret = mlag(ret, -1)

MKVAL = MKVAL[month.ends,]

for(j in 1:len(factors)) {  
  for(i in 1:len(factors[[j]])) {
    factors[[j]][[i]] = factors[[j]][[i]][month.ends,]  
  }
}

# To create an overall Traditional Value factor, let’s first normalize (convert to z scores) 
# all Traditional Value factors by subtracting capitalization weighted average and dividing by standard deviation. 
# The overall Traditional Value factor is an average of all normalized Traditional Value factors.

#*****************************************************************
# Create the overall Traditional Value factor 
#****************************************************************** 
# check missing data for financial firms
sapply(factors$TV, count)   

# normalize (convert to z scores) cross sectionaly all Traditional Value factors
for(i in names(factors$TV)) {
  factors$TV[[i]] = (factors$TV[[i]] - cap.weighted.mean(factors$TV[[i]], MKVAL)) / 
    apply(factors$TV[[i]], 1, sd, na.rm=T)
}

# compute the overall Traditional Value factor
load.packages('abind') 
temp = abind(factors$TV, along = 3)
factors$TV$AVG = factors$TV[[1]]
factors$TV$AVG[] = apply(temp, c(1,2), mean, na.rm=T)

# plot quintile charts for all Traditional Value factors
layout(matrix(1:6,nc=2))
sapply(1:len(factors$TV), function(i)
  compute.quantiles(factors$TV[[i]], next.month.ret, paste(names(factors$TV)[i], 'Traditional Value'))
)

#I created a compute.quantiles() function in factor.model.r at github to compute and plot quantiles. 
#For example, the quantiles chart for EP factor shows the average next month performance of stocks in each quantiles. 
#The quantiles are created each month by ranking stocks by EP factor and grouping them into 5 quantiles. 
#There is tendency of quantile 5 (Q5) to outperform quantile 1 (Q1) in most cases. 
#The relationship between quantiles is not perfect, but the spread between Q5-Q1 is positive.

#Next let’s examine quantiles for the overall Traditional Value factor in more details.

#*****************************************************************
# Backtest quantiles and quantile spread
#****************************************************************** 
out = compute.quantiles(factors$TV$AVG, next.month.ret, plot=F) 

prices = data$prices
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))

# create strategies that invest in each qutile
models = list()

for(i in 1:5) {
  data$weight[] = NA
  data$weight[month.ends,] = iif(out$quantiles == i, out$weights, 0)
  capital = 100000
  data$weight[] = (capital / prices) * (data$weight)  
  models[[paste('Q',i,sep='')]] = bt.run(data, type='share', capital=capital)
}

# spread
data$weight[] = NA
data$weight[month.ends,] = iif(out$quantiles == 5, out$weights, 
                               iif(out$quantiles == 1, -out$weights, 0))
capital = 100000
data$weight[] = (capital / prices) * (data$weight)
models$Q5_Q1 = bt.run(data, type='share', capital=capital)

#*****************************************************************
# Create Report
#****************************************************************** 
plotbt(models, plotX = T, log = 'y', LeftMargin = 3)            
mtext('Cumulative Performance', side = 2, line = 1)

#The quantile spread Q5-Q1 shows consistent positive performance after 2000, but is inverted from 1995 to 2000. 
#This is a bit strange and calls for more investigation.

#In the next posts, I will show how to run pooled cross sectional regression to create alpha scores.


