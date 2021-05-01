#####################################################################################
# Multiple Factor Model Summary
# https://systematicinvestor.wordpress.com/2012/03/11/multiple-factor-model-summary/
#####################################################################################


#==================================================================================================
# Multiple Factor Model – Building CSFB Factors
# https://systematicinvestor.wordpress.com/2012/01/29/multiple-factor-model-fundamental-data/
#==================================================================================================
# To save time not to download data from web again, you can reload the data starting running the command in line 512 or
# line 912 to reload composite factor returns
#####################################
# 3 post starts from line 470
#####################################
#####################################
# 4 post starts from line 1028
#####################################

###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

load.packages('abind') 
load.packages('quantmod')
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
# save(fund, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/fund.Rdata')
# load(file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/fund.Rdata')

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

# get pricing data
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)    

# combine fundamental and pricing data
# i="AAPL"
for(i in tickers) {
  fund = data.fund[[i]]
  fund.date = date.fund.data(fund)
  
  EPS.Q = as.double(fund['Diluted EPS from Total Operations',])
  EPS.Q = as.xts(EPS.Q, fund.date)    
  EPS = runSum(EPS.Q, 4)
  
  data[[i]] = merge(data[[i]], EPS)
}

bt.prep(data, align='keep.all', dates='1995::2014')

save(data, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.Rdata')
# save the fundamental data for future use
save(data.fund, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.fund.Rdata')

#It takes a while to download historical fundamental data for all companies in the Dow Jones index, 
#so I recommend saving your results with save(data.fund, file=’data.fund.Rdata’) command. 
#Later on if you want to run code one more time, just load(file=’data.fund.Rdata’) instead of downloading all data again.

##############################################################################################
# Reload data
load(file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.fund.Rdata')
load(file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.Rdata')
###############################################################################################

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
#Vo(data)
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

###########################################################################################################
# This is the second post in the series about Multiple Factor Models. I will build on the code presented 
#in the prior post, Multiple Factor Model – Fundamental Data, and I will show how to build Fundamental factors 
#described in the CSFB Alpha Factor Framework. For details of the CSFB Alpha Factor Framework please read 
#CSFB Quantitative Research, Alpha Factor Framework on page 11, page 49 by P. N. Patel, S. Yao, R. Carlson, A. Banerji, J. Handelman.

#The CSFB Alpha Factor Framework has both traditional Fundamental factors and industry relative Fundamental factors.
#Let’s start by getting Fundamental data that we will need to create Price/Earnings, Price/Sales, Price/Cash Flow,
#Dividend Yield, Price/Book factors.

# https://systematicinvestor.wordpress.com/2012/02/04/multiple-factor-model-building-fundamental-factors/
###########################################################################################################

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
#load(file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.fund.Rdata')

# get pricing data
#load(file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.Rdata')

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

# save Traditional Value factor (not normalized yet)
save(factors, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/factors.TV.Rdata')


# To create an overall Traditional Value factor, let’s first normalize (convert to z scores) 
# all Traditional Value factors by subtracting capitalization weighted average and dividing by standard deviation. 
# The overall Traditional Value factor is an average of all normalized Traditional Value factors.

#*****************************************************************
# Create the overall Traditional Value factor 
#****************************************************************** 
# check missing data for financial firms
sapply(factors$TV, count)   

# normalize (convert to z scores) cross sectionaly all Traditional Value factors
# names(factors$TV)
# [1] "EP"  "SP"  "CFP" "DY"  "BP" 

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
# Here we use average of six traditional factors, which is denoted as AVG
out = compute.quantiles(factors$TV$AVG, next.month.ret, plot=F) 

#library(WriteXLS)
#load.packages(WriteXLS)
#library(rJava)
#library(xlsx)
#load.packages('xlsx')

###################################################################################
# A function that Quickly export multiple R objects to an Excel Workbook
# But not working properly because of pakcage rJava is not properly installed. 
# save.xlsx <- function (file, ...)
# {
#   require(xlsx, quietly = TRUE)
#   objects <- list(...)
#   fargs <- as.list(match.call(expand.dots = TRUE))
#   objnames <- as.character(fargs)[-c(1, 2)]
#   nobjects <- length(objects)
#   for (i in 1:nobjects) {
#     if (i == 1)
#       write.xlsx(objects[[i]], file, sheetName = objnames[i])
#     else write.xlsx(objects[[i]], file, sheetName = objnames[i],
#                     append = TRUE)
#   }
#   print(paste("Workbook", file, "has", nobjects, "worksheets."))
# }
# 
# save.xlsx('D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/out.xlsx',
#           out$quantiles, out$weights, out$ranking, out$hist.factor.quantiles, out$hist.ret.quantiles)
#########################################################################################################

#write.zoo(factors$TV$AVG, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/factors_TV_AVG.csv', index.name="Date", sep=',')
#write.zoo(next.month.ret, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/next_month_ret.csv',index.name="Date", sep=',')
#write.zoo(out$quantiles, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/out_quantiles.csv',index.name="Date", sep=',')
#write.csv(out$quantiles, 'D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/out_quantiles.csv')
# write.csv(out$weights, 'D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/out_weights.csv')
# write.csv(out$ranking, 'D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/out_ranking.csv')
# write.csv(out$hist.factor.quantiles, 'D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/out_hist_fact_quantiles.csv')
# write.csv(out$hist.ret.quantiles, 'D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/out_hist_ret_quantiles.csv')


prices = data$prices
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))

# create strategies that invest in each qutile
models = list()
# i=1
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


save(data, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.Rdata')
# save the fundamental data for future use
save(data.fund, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.fund.Rdata')


#The quantile spread Q5-Q1 shows consistent positive performance after 2000, but is inverted from 1995 to 2000. 
#This is a bit strange and calls for more investigation.

###########################################################################################################
#In the next posts, I will show how to run pooled cross sectional regression to create alpha scores.
#############################################################################################################################
#This is the third post in the series about Multiple Factor Models. 
#I will build on the code presented in the prior post, Multiple Factor Model – Building Fundamental Factors, 
#and I will show how to build majority of factors described in the CSFB Alpha Factor Framework. For details of the CSFB Alpha Factor Framework please read CSFB Quantitative Research, Alpha Factor Framework on page 11, page 49 by P. N. Patel, S. Yao, R. Carlson, A. Banerji, J. Handelman.
#This post will include long sections of code to extract/format data and build factors. 
#I created a few helper functions for data manipulations and visualizations in the factor.model.r at github. 
#For example, consecutive.changes function counts the number of consecutive positive changes.

#The outline of this post:
  
# Create majority of CSFB factors
# Create and test Composite Average factor
# Run cross sectional regression to estimate factor loading
# Create and test Alpha model using estimated factor loading

# https://systematicinvestor.wordpress.com/2012/02/13/multiple-factor-model-building-csfb-factors/
#############################################################################################################################

#*****************************************************************
# Find Sectors for each company in DOW 30
#****************************************************************** 
tickers = spl('XLY,XLP,XLE,XLF,XLV,XLI,XLB,XLK,XLU')
tickers.desc = spl('ConsumerCyclicals,ConsumerStaples,Energy,Financials,HealthCare,Industrials,Materials,Technology,Utilities')

sector.map = c()
# i = 1
for(i in 1:len(tickers)) {
  sector.map = rbind(sector.map, 
                     cbind(sector.spdr.components(tickers[i]), tickers.desc[i])
  )
}
colnames(sector.map) = spl('ticker,sector')

#*****************************************************************
# Load historical data
#******************************************************************
load.packages('quantmod,abind')     
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
  nperiods = ncol(fund)
  
  # D - holds all data to be merged with pricing data
  D = list()
  
  #--------------------------------------------------------------
  # Data for Traditional and Relative Value   
  #--------------------------------------------------------------
  
  # Earnings per Share        
  D$EPS = get.fund.data('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling=T)
  
  # Sales, exception not available for financial service firms
  D$SALE = get.fund.data('total revenue', fund, fund.date, is.12m.rolling=T)
  
  # Common Shares Outstanding
  D$CSHO = get.fund.data('total common shares out', fund, fund.date)
  
  # Common Equity
  D$CEQ = get.fund.data('total equity', fund, fund.date)
  
  # Dividends
  D$DV.PS = get.fund.data('dividends paid per share', fund, fund.date, is.12m.rolling=T)
  
  # Cash Flow
  D$CFL = get.fund.data('net cash from operating activities', fund, fund.date, cash.flow=T, is.12m.rolling=T)
  
  #--------------------------------------------------------------
  # Data for Historical Growth    
  #--------------------------------------------------------------
  
  # Consecutive Quarters of Positive Changes in Trailing 12 Month Cash Flow
  D$CFL.CON.CHG = consecutive.changes(D$CFL)      
  # check
  #merge(D$CFL, sign(diff(D$CFL)), consecutive.changes(D$CFL), consecutive.changes(D$CFL,F))
  
  # Consecutive Quarters of Positive Change in Quarterly Earnings
  D$EPS.CON.CHG = consecutive.changes(D$EPS)
  
  # 12 Month Change in Quarterly Cash Flow
  temp = get.fund.data('net cash from operating activities', fund, fund.date, cash.flow=T)
  D$CFL.CHG = temp / mlag(temp,4)
  
  # 3 Year Average Annual Sales Growth
  D$SALE.3YR.GR = D$SALE
  if(!all(is.na(D$SALE))) D$SALE.3YR.GR = SMA(ifna(D$SALE / mlag(D$SALE,4) - 1,NA), 3*4)
  
  # 3 Year Average Annual Earnings Growth
  D$EPS.3YR.GR = SMA(D$EPS / mlag(D$EPS,4) - 1, 3*4)
  
  # 12 Quarter Trendline in Trailing 12 Month Earnings        
  D$EPS.TREND = D$EPS * NA
  D$EPS.TREND[12:nperiods] = sapply(12:nperiods, function(i) beta.degree(ols(cbind(1,1:12), D$EPS[(i-12+1):i])$coefficients[2]))
  
  # Slope of Trend Line Through Last 4 Quarters of Trailing 12M Cash Flows        
  D$CFL.TREND = D$CFL * NA
  D$CFL.TREND[4:nperiods] = sapply(4:nperiods, function(i) beta.degree(ols(cbind(1,1:4), D$CFL[(i-4+1):i])$coefficients[2]))
  
  #--------------------------------------------------------------
  # Data for Profit Trends    
  #--------------------------------------------------------------
  RECT = get.fund.data('receivables', fund, fund.date)
  INVT = get.fund.data('inventories', fund, fund.date)
  
  D$AT = get.fund.data('total assets', fund, fund.date)
  XSGA = get.fund.data('Selling, General & Administrative (SG&A) Expense', fund, fund.date, is.12m.rolling=T)
  
  # Consecutive Quarters of Declines in (Receivables+Inventories) / Sales
  D$RS.CON.CHG = consecutive.changes((RECT + INVT) / D$SALE, F)
  
  # Consecutive Qtrs of Positive Change in Trailing 12M Cash Flow / Sales
  D$CS.CON.CHG = consecutive.changes(D$CFL/D$SALE)
  
  # Overhead = sales, general and administrative costs
  # Consecutive Quarters of Declines in Trailing 12 Month Overhead / Sales
  D$OS.CON.CHG = consecutive.changes(XSGA/D$SALE, F)
  
  # (Industry Relative) Trailing 12 Month (Receivables+Inventories) / Sales
  D$RS = (RECT + INVT) / D$SALE
  
  # (Industry Relative) Trailing 12 Month Sales / Assets
  D$SA = D$SALE / D$AT
  
  # Trailing 12 Month Overhead / Sales
  D$OS = XSGA / D$SALE
  
  # Trailing 12 Month Earnings / Sales
  D$ES = D$EPS / D$SALE                       
  
  #--------------------------------------------------------------
  # Merge Fundamental and Pricing data
  #--------------------------------------------------------------
  
  # merge 
  data[[i]] = merge(data[[i]], as.xts(abind(D,along=2), fund.date))                       
}
# i="AAPL"
bt.prep(data, align='keep.all', dates='1995::2014')

# save 30 stocks data into different files
for(i in tickers) {
  filename1 = paste("D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/", i, sep="")
  filename2 = paste(filename1, ".csv", sep="")
    write.csv(as.data.frame(data[[i]]), file =filename2)
}

#*****************************************************************
# Create Factors
#****************************************************************** 
prices = data$prices    
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))

# re-map sectors
sectors = sectors[colnames(prices)] 

# create factors
factors = list()
factor.names = list()   

################################################
#Next let’s create majority of CSFB factors.
################################################

#*****************************************************************
# Create Traditional Value
#****************************************************************** 
factors$TV = list()
factor.names$TV = 'Traditional Value'

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
# Create Historical Growth
#****************************************************************** 
factors$HG = list()
factor.names$HG = 'Historical Growth'

for(i in spl('CFL.CON.CHG,EPS.CON.CHG,CFL.CHG,SALE.3YR.GR,EPS.3YR.GR,EPS.TREND,CFL.TREND')) {
  factors$HG[[i]] = bt.apply(data, function(x) ifna.prev(x[, i]))
}

#*****************************************************************
# Create Profit Trends
#****************************************************************** 
factors$PT = list()     
factor.names$PT = 'Profit Trends'  

for(i in spl('RS.CON.CHG,CS.CON.CHG,OS.CON.CHG,RS,SA,OS,ES')) {
  factors$PT[[i]] = bt.apply(data, function(x) ifna.prev(x[, i]))
}

#*****************************************************************
# Create Price Momentum
#****************************************************************** 
factors$PM = list()
factor.names$PM = 'Price Momentum' 

# find week ends
week.ends = endpoints(prices, 'weeks')
week.prices = prices[week.ends,]
week.nperiods = nrow(week.prices)

#Slope of 52 Week Trend Line
factors$PM$S52W.TREND = bt.apply.matrix(week.prices, function(x) {
  c(rep(NA,51),
    sapply(52:week.nperiods, function(i) beta.degree(ols(cbind(1,1:52), x[(i - 52 + 1):i])$coefficients[2]))
  )})

#4/52 Week Price Oscillator
factors$PM$PP04.52W = bt.apply.matrix(week.prices, EMA, 4) / bt.apply.matrix(week.prices, EMA, 52)

#39 Week Return
factors$PM$R39W = week.prices / mlag(week.prices, 39)

#51 Week Volume Price Trend
# compute weekly volume
temp = bt.apply(data, function(x) cumsum(ifna(Vo(x),0)))
temp = temp[week.ends,]
week.volume = temp - mlag(temp)     
temp = (week.prices - mlag(week.prices)) * week.volume
factors$PM$VPT51W = bt.apply.matrix(temp, runSum, 51)

# Convert weekly to daily
for(i in 1:len(factors$PM)) {
  temp = prices * NA
  temp[week.ends,] = factors$PM[[i]]
  factors$PM[[i]] = bt.apply.matrix(temp, function(x) ifna.prev(x))
}

#Percent Above 260 Day Low
factors$PM$P260LOW = prices / bt.apply.matrix(prices, runMin, 260)

# Flip sign
for(i in names(factors$PM)) factors$PM[[i]] = -factors$PM[[i]]

#*****************************************************************
# Create Price Reversal
#****************************************************************** 
factors$PR = list()
factor.names$PR = 'Price Reversal' 

#5 Day Industry Relative Return
factors$PR$r5DR = prices/mlag(prices, 5)
factors$PR$r5DR = factors$PR$r5DR / sector.mean(factors$PR$r5DR, sectors)

#5 Day Money Flow / Volume
factors$PR$MFV = bt.apply(data, function(x) {
  MFI(cbind(ifna.prev(Hi(x)),ifna.prev(Lo(x)),ifna.prev(Cl(x))), 5) / ifna.prev(Vo(x))
})

#10 Day MACD - Signal Line
factors$PR$MACD = bt.apply.matrix(prices, function(x) {
  temp=MACD(x, 10)
  temp[, 'macd'] - temp[, 'signal']
})      

#14 Day RSI (Relative Strength Indicator)
factors$PR$RSI = bt.apply.matrix(prices, RSI, 14)

#14 Day Stochastic
factors$PR$STOCH = bt.apply(data, function(x) {
  stoch(cbind(ifna.prev(Hi(x)),ifna.prev(Lo(x)),ifna.prev(Cl(x))),14)[,'slowD']
})

#4 Week Industry Relative Return
factors$PR$rR4W = week.prices / mlag(week.prices,4)
factors$PR$rR4W = factors$PR$rR4W / sector.mean(factors$PR$rR4W, sectors)

# Convert weekly to daily
temp = prices * NA
temp[week.ends,] = factors$PR$rR4W
factors$PR$rR4W = bt.apply.matrix(temp, function(x) ifna.prev(x))


# VOMO - Volume x Momentum
volume = bt.apply(data, function(x) ifna.prev(Vo(x)))
factors$PR$VOMO = (prices / mlag(prices,10) - 1) * bt.apply.matrix(volume, runMean, 22) / bt.apply.matrix(volume, runMean, 66)

# Flip sign
for(i in names(factors$PR)) factors$PR[[i]] = -factors$PR[[i]]

#*****************************************************************
# Create Small Size
#******************************************************************         
factors$SS = list()
factor.names$SS = 'Small Size'     

#Log of Market Capitalization
factors$SS$MC = log(MKVAL)

#Log of Market Capitalization Cubed
factors$SS$MC3 = log(MKVAL)^3

#Log of Stock Price
factors$SS$P = log(prices)

#Log of Total Assets
factors$SS$AT = log(bt.apply(data, function(x) ifna.prev(x[, 'AT'])))

#Log of Trailing-12-Month Sales
factors$SS$SALE = log(bt.apply(data, function(x) ifna.prev(x[, 'SALE'])))

# Flip sign
for(i in names(factors$SS)) factors$SS[[i]] = -factors$SS[[i]]

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
    factors[[j]][[i]][] = ifna(factors[[j]][[i]],NA)
  }
}

#*****************************************************************
# Create Relative Value
#****************************************************************** 
factors$RV = list()
factor.names$RV = 'Relative Value'     

# relative 
for(i in spl('EP,SP,CFP')) {
  factors$RV[[paste('r',i,sep='')]] = factors$TV[[i]] / sector.mean(factors$TV[[i]], sectors)         
}

# spreads, 5 Year Avg = 60 months
for(i in spl('rEP,rSP,rCFP')) {
  factors$RV[[paste('s',i,sep='')]] = factors$RV[[i]] - 
    apply(factors$RV[[i]], 2, function(x) if(all(is.na(x))) x else SMA(x,60) )
}

#*****************************************************************
# Profit Trends (Relative)
#******************************************************************     
# relative 
for(i in spl('RS,SA')) {
  factors$PT[[paste('r',i,sep='')]] = factors$PT[[i]] / sector.mean(factors$PT[[i]], sectors)
}           

#Next let’s create Composite Average factor and chart its performance.


#*****************************************************************
# Normalize and add Average factor
#****************************************************************** 
#names(factors)
#[1] "TV" "HG" "PT" "PM" "PR" "SS" "RV"

for(j in names(factors)) {
  factors[[j]] = normalize.normal(factors[[j]])
  factors[[j]] = add.avg.factor(factors[[j]])
}

temp = abind(factors$TV, along = 3)


#*****************************************************************
# Create Composite Average factor
#******************************************************************     
factors.avg = list()
for(j in names(factors)) factors.avg[[j]] = factors[[j]]$AVG

factors.avg = add.avg.factor(factors.avg)

#*****************************************************************
# Backtest qutiles and qutile spread
#****************************************************************** 
plot.quantiles(factors.avg, next.month.ret, 'Average')

plot.bt.quantiles(factors.avg$AVG, next.month.ret, 'Composite Average', data)

#########################################################################################################
#Please note that I’m using the current Dow Jones index components through out the whole history. 
#This is a problem because the Dow Jones index changed its composition a few times in the last 20 years. 
#One of the signs of this bias is high spread for Small Size group of factors. It is not obvious that buying 
#low priced stocks and selling high priced stocks should consistently make money; but it makes sense, 
#if we know beforehand that that low priced companies will be part of Dow Jones index at some point.

#Instead of using a simple average of all factors to rank stocks, we can run cross sectional regression to estimate 
#factor loading, and create Alpha model using estimated factor loading. For a complete description of this process, 
#I recommend reading Commonality In The Determinants Of Expected Stock Returns by R. Haugen, N. Baker (1996) pages 8-9.
#########################################################################################################

#*****************************************************************
# Save CSFB factors to be used later to create multiple factor Risk Model
#****************************************************************** 
save(data, sectors, factors.avg, next.month.ret, file="D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.factors.Rdata")
# remove Composite Average factor
# load("D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.factors.Rdata")
factors.avg = factors.avg[which(names(factors.avg) != 'AVG')]

#*****************************************************************
# Run cross sectional regression and create Alpha model
#****************************************************************** 
nperiods = nrow(next.month.ret)
n = ncol(next.month.ret)

#factors.avg1 = factors.avg
#factors.avg1$Ret = next.month.ret
#filename = 'D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/factors_'

# for (i in 1:length(ls(factors.avg1))){
#   filename1 = paste(filename, names(factors.avg1)[i], sep='')
#   filename2 = paste(filename1, '.csv', sep='')
#   write.zoo(factors.avg1[[i]], file=filename2, index.name="Date", sep=',')
# }


# > class(factors.avg)
# [1] "list"
# create matrix for each factor
# > dim(factors.matrix) 
# [1] 240  30   7
# names(factors.matrix[1,1,])
factors.matrix = abind(factors.avg, along = 3)  
all.data = factors.matrix

# betas
beta = all.data[,1,] * NA

# append next.month.ret to all.data         
all.data = abind(next.month.ret, all.data, along = 3)
dimnames(all.data)[[3]][1] = 'Ret'
# all.data = 240*30*8; 240 dates by 30 companies by 8 factor returns; 

# save array all.data 
#save(all.data, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/all.data.Rdata')
#load(file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/all.data.Rdata')

# estimate betas (factor returns)
#t=1
#for(t in 12:(nperiods-1)) { # we change from 12 to 20 because there are some NA in factor returns
for(t in 20:(nperiods-1)) {
  temp = all.data[t:t,,]
  x = temp[,-1]
  y = temp[,1]
  beta[(t+1),] = lm(y~x-1)$coefficients  # -1 means omitting intercept
}


# create Alpha return forecasts
alpha = next.month.ret * NA

#for(t in 18:(nperiods-1)) {
# t = 26
for(t in 26:(nperiods-1)) {
  # average betas over the last 6 months
  coef = colMeans(beta[(t-5):t,],na.rm=T)
  alpha[t,] = rowSums(all.data[t,,-1] * t(repmat(coef, 1,n)), na.rm=T)    
}

#*****************************************************************
# Backtest qutiles and qutile spread
#****************************************************************** 
setEPS()
layout(1:2)
postscript("D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/alpha_1.eps", horizontal = FALSE, onefile = FALSE, paper = "special",
           colormodel = "rgb") 
temp = compute.quantiles(alpha, next.month.ret, plot=T)

plot.bt.quantiles(alpha, next.month.ret, 'Alpha', data)
#plotbt.custom.report.part1(models.tw[spl('twse,equal.weight,spread,M1_Q1,M1_Q2,M1_Q3')])
dev.off()

#####################################################################
# Use factor loadings that are positive to construct Alpha score
######################################################################
#The performance of the regression model lags the performance of the simple average of all factors to rank stocks. 
#There are might be many reasons for this, but I want to show you one quick and rational way to increase performance 
#of the regression model.

#We do not restrict estimated factor loadings during regression; however, a negative coefficient for a Value factor 
#does not make sense. I don’t want explicitly say that good Value companies should have lower ranks than bad Value 
#companies, just because there is a negative coefficient for a Value factor. One possible solution is to only use 
#factor loadings that are positive to construct Alpha score. Here is the modified code:

for(t in 26:(nperiods-1)) {
  # average betas over the last 6 months
  coef = colMeans(beta[(t-5):t,],na.rm=T)
  coef = iif(coef > 0, coef, 0)
  alpha[t,] = rowSums(all.data[t,,-1] * t(repmat(coef, 1,n)), na.rm=T)    
}

setEPS()
layout(1:2)
postscript("D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/alpha_1.eps", horizontal = FALSE, onefile = FALSE, paper = "special",
           colormodel = "rgb") 
temp = compute.quantiles(alpha, next.month.ret, plot=T)

plot.bt.quantiles(alpha, next.month.ret, 'Alpha', data)
#plotbt.custom.report.part1(models.tw[spl('twse,equal.weight,spread,M1_Q1,M1_Q2,M1_Q3')])
dev.off()

######################################################################################################################
#This is the fourth post in the series about Multiple Factor Models. I will build on the code presented in the prior
#post, Multiple Factor Model – Building CSFB Factors, and I will show how to build a multiple factor risk model.
#For an example of the multiple factor risk models, please read following references:
  
# MSCI Barra United States Equity Multi-Factor Model, page 101
# Northfield Fundamental Risk Model
# The outline of this post:
  
# Run cross sectional regression to estimate factor returns
# Compute factor covariance using shrinkage estimator
# Forecast stocks specific variances using GARCH(1,1)
# Compute portfolio risk using multiple factor model and compare it to the historical standard deviation of
# portfolio returns.
# Let’s start by loading the CSFB factors that we saved at the end of the prior post. 
# [If you are missing data.factors.Rdata file, please execute fm.all.factor.test() function first to create and 
# save CSFB factors.] Next, I will run cross sectional regression to estimate factor returns.

######################################################################################################################
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)
#*****************************************************************
# Load factor data that we saved at the end of the fm.all.factor.test functions
#****************************************************************** 
load.packages('quantmod,abind') 

load("D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/data.factors.Rdata")
# remove Composite Average factor
factors.avg = factors.avg[which(names(factors.avg) != 'AVG')]  

#*****************************************************************
# Run cross sectional regression to estimate factor returns
#****************************************************************** 
nperiods = nrow(next.month.ret)
n = ncol(next.month.ret)

# create sector dummy variables: binary 0/1 values for each sector
nsectors = len(levels(sectors)) 
sectors.matrix = array(double(), c(nperiods, n, nsectors))
dimnames(sectors.matrix)[[3]] = levels(sectors)     
for(j in levels(sectors)) {
  sectors.matrix[,,j] = matrix(sectors == j,  nr=nperiods, nc=n, byrow=T)
}

# create matrix for each factor
factors.matrix = abind(factors.avg, along = 3)      

# combine sector dummies and all factors
all.data = abind(sectors.matrix, factors.matrix)        

# create betas and specific.return
beta = all.data[,1,] * NA
specific.return = next.month.ret * NA
nfactors = ncol(beta)

# append next.month.ret to all.data         
all.data = abind(next.month.ret, all.data, along = 3)
dimnames(all.data)[[3]][1] = 'Ret'

# estimate betas (factor returns)
# t=20
for(t in 20:(nperiods-1)) {     
  temp = all.data[t:t,,]
  x = temp[,-c(1:2)]
  y = temp[,1]
  b = lm(y~x)$coefficients
  
  b[2:nsectors] = b[1] + b[2:nsectors] #???
  beta[(t+1),] = b        
  
  specific.return[(t+1),] = y - rowSums(temp[,-1] * matrix(b, n, nfactors, byrow=T), na.rm=T) 
}

#Note that we cannot include the first sector dummy variable in the regression, otherwise we will get a linearly 
#dependent relationship of the first sector dummy variable with all other sector dummy variables. 
#The sector effect of the first sector dummy variable is absorbed into the intercept in the regression.

#There are a few alternative ways of estimating this regression. For example, the robust linear model 
#can be estimated with following code:

load.packages('MASS')

for(t in 20:(nperiods-1)) {     
  temp = all.data[t:t,,]
  x = temp[,-c(1:2)]
  y = temp[,1]
  temp = rlm(y~x)$coefficients
  
  b[2:nsectors] = b[1] + b[2:nsectors] #???
  beta[(t+1),] = b        
  
  specific.return[(t+1),] = y - rowSums(temp[,-1] * matrix(b, n, nfactors, byrow=T), na.rm=T) 
}

#The quantile regression can can be estimated with following code:
load.packages('quantreg')
# 
for(t in 20:(nperiods-1)) {     
  temp = all.data[t:t,,]
  x = temp[,-c(1:2)]
  y = temp[,1]
  temp = rq(y ~ x, tau = 0.5)$coefficients
  
  b[2:nsectors] = b[1] + b[2:nsectors] #???
  beta[(t+1),] = b        
  
  specific.return[(t+1),] = y - rowSums(temp[,-1] * matrix(b, n, nfactors, byrow=T), na.rm=T) 
}

#*****************************************************************
# helper function
#******************************************************************     
fm.hist.plot <- function(temp, smain=NULL) {         
  ntemp = ncol(temp)      
  cols = plota.colors(ntemp)  
  plota(temp, ylim = range(temp), log='y', main=smain)
  for(i in 1:ntemp) plota.lines(temp[,i], col=cols[i])
  plota.legend(colnames(temp), cols, as.list(temp))
}


#*****************************************************************
# Examine historical cumulative factor returns
#******************************************************************     
temp = make.xts(beta, index(next.month.ret))
temp = temp['2000::',]
temp[] = apply(coredata(temp), 2, function(x) cumprod(1 + ifna(x,0)))

fm.hist.plot(temp[,-c(1:nsectors)], 'Factor Returns')

#Next let’s estimate the factor covariance matrix over the rolling 24 month window.

load.packages('BurStFin')   
factor.covariance = array(double(), c(nperiods, nfactors, nfactors))
dimnames(factor.covariance)[[2]] = colnames(beta)
dimnames(factor.covariance)[[3]] = colnames(beta)

# estimate factor covariance
# t=44
for(t in 44:nperiods) {
  factor.covariance[t,,] = var.shrink.eqcor(beta[(t-23):t,])
}

#Next let’s forecast stocks specific variances using GARCH(1,1). I will use the GARCH estimation routine described 
#in the Trading using Garch Volatility Forecast post.

#*****************************************************************
# Compute stocks specific variance foreasts using GARCH(1,1)
#******************************************************************     
load.packages('tseries,fGarch') 

specific.variance = next.month.ret * NA

for(i in 1:n) {
  specific.variance[,i] = bt.forecast.garch.volatility(specific.return[,i], 24) 
}

# compute historical specific.variance
hist.specific.variance = next.month.ret * NA
for(i in 1:n) hist.specific.variance[,i] = runSD(specific.return[,i], 24)   

# if specific.variance is missing use historical specific.variance
specific.variance[] = ifna(coredata(specific.variance), coredata(hist.specific.variance))   

#*****************************************************************
# Save multiple factor risk model to be used later during portfolio construction
#****************************************************************** 
save(all.data, factor.covariance, specific.variance, file='D:/亞洲大學上課資料/Portfolio management 2015 Fall/MFM/risk.model.Rdata')

#Portfolio Risk = (common factor variance + specific variance)^0.5
#common factor variance = (portfolio factor exposure) * factor covariance matrix * (portfolio factor exposure)'
#specific variance = (specific.variance)^2 * (portfolio weights)^2

#*****************************************************************
# Compute portfolio risk
#****************************************************************** 
portfolio = rep(1/n, n)
portfolio = matrix(portfolio, n, nfactors)

portfolio.risk = next.month.ret[,1] * NA
for(t in 36:(nperiods-1)) { 
  portfolio.exposure = colSums(portfolio * all.data[t,,-1], na.rm=T)
  
  portfolio.risk[t] = sqrt(
    portfolio.exposure %*% factor.covariance[t,,] %*% (portfolio.exposure) + 
      sum(specific.variance[t,]^2 * portfolio[,1]^2, na.rm=T)
  )
}

#Next let’s compare portfolio risk estimated using multiple factor risk model with portfolio historical risk.
#*****************************************************************
# Compute historical portfolio risk
#****************************************************************** 
portfolio = rep(1/n, n)
portfolio = t(matrix(portfolio, n, nperiods))

portfolio.returns = next.month.ret[,1] * NA
portfolio.returns[] = rowSums(mlag(next.month.ret) * portfolio, na.rm=T)

hist.portfolio.risk = runSD(portfolio.returns, 24)

#*****************************************************************
# Plot risks
#******************************************************************             
plota(portfolio.risk['2000::',], type='l')
plota.lines(hist.portfolio.risk, col='blue')
plota.legend('Risk,Historical Risk', 'black,blue')

#The multiple factor risk model does a decent job of estimating portfolio risk most of the time.



