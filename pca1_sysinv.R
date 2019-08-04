#https://systematicinvestor.wordpress.com/?s=principal+component
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
load.packages('quantmod')
tickers = dow.jones.components()

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '2010-01-01', env = data, auto.assign = T)
bt.prep(data, align='remove.na')  
data$AAPL
#*****************************************************************
# Principal component analysis (PCA), for interesting discussion
# http://machine-master.blogspot.ca/2012/08/pca-or-polluting-your-clever-analysis.html
#****************************************************************** 
dim(data$prices)
prices = last(data$prices, 1000)
n = len(tickers)        
ret = prices / mlag(prices) - 1
dim(ret)
p = princomp(na.omit(ret[1:95,]))

loadings = p$loadings[]

# look at the first 4 principal components  
components = loadings[,1:4]

# normalize all selected components to have total weight = 1
library(Jmisc)
components = components / repRow(colSums(abs(components)), len(tickers))
colSums(components)
# note that first component is market, and all components are orthogonal i.e. not correlated to market
market = ret[1:95,] %*% rep(1/n,n)
temp = cbind(market, ret[1:250,] %*% components)
colnames(temp)[1] = 'Market'   

round(cor(temp, use='complete.obs',method='pearson'),2)

# the variance of each component is decreasing
round(100*sd(temp,na.rm=T),2)



