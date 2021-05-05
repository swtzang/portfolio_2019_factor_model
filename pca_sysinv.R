# https://systematicinvestor.wordpress.com/?s=principal+component
# https://systematicinvestor.wordpress.com/2012/12/22/visualizing-principal-components/
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
#setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Find Sectors for each company in DOW 30
#****************************************************************** 
tickers = spl('XLC,XLY,XLP,XLE,XLF,XLRE,XLV,XLI,XLB,XLK,XLU')
tickers.desc = spl('CommunicationServices,
                   ConsumerDiscretionary, 
                   ConsumerStaples,
                   Energy,
                   Financials,
                   RealEstate,
                   HealthCare,
                   Industrials,
                   Materials,
                   Technology,
                   Utilities')
# https://www.sectorspdr.com/sectorspdr/sector/xlc/portfolio

sector.spdr.components <- function(sector.etf = 'XLE') {
  url = paste('http://www.sectorspdr.com/sectorspdr/IDCO.Client.Spdrs.Portfolio/Export/ExportCsv?symbol=', sector.etf, sep='')
  temp = read.csv(url, skip=1, header=TRUE, stringsAsFactors=F)
  tickers = temp[, 'Symbol']
  return(tickers)
}


sector.map = c()
# i = 1
for(i in 1:len(tickers)) {
  sector.map = rbind(sector.map, 
                     cbind(sector.spdr.components(tickers[i]), tickers.desc[i])
  )
}
colnames(sector.map) = spl('ticker,sector')
dim(sector.map)
# delete extra space from the strings 
industry <- gsub("\\s", "", sector.map[,2])
sector.map[,2] <- industry
sector.map
#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')   
tickers = dow.jones.components() # this function is not working because yahoo finance webpage is no longer providing dj30 components
# url = 'http://finance.yahoo.com/q/cp?s=^DJI+Components'
url = 'https://www.dividendmax.com/market-index-constituents/dow-jones-30'
txt = join(readLines(url))
temp = extract.table.from.webpage(txt, 'Volume', has.header = T)
temp <- temp[-nrow(temp),]
tickers <- temp[,2]

sectors = factor(sector.map[ match(tickers, sector.map[,'ticker']), 'sector'])
names(sectors) = tickers
# 
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '2000-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)    

bt.prep(data, align='keep.all', dates='2012')

# re-order sectors, because bt.prep can change the order of tickers
sectors = sectors[data$symbolnames]

# save data for later examples
save(data, tickers, sectors, file='bt.pca.test.Rdata')
bt.pca.test.data <- load("bt.pca.test.Rdata")
#*****************************************************************
# Principal component analysis (PCA), for interesting discussion
# http://machine-master.blogspot.ca/2012/08/pca-or-polluting-your-clever-analysis.html
#****************************************************************** 
prices = data$prices    
ret = prices / mlag(prices) - 1
dim(ret)
tail(ret,1)
ret <- ret[-1,] 
ret <- ret[,-7]
p = princomp(na.omit(ret))

loadings = p$loadings[]
p.variance.explained = p$sdev^2 / sum(p$sdev^2)

# plot percentage of variance explained for each principal component    
barplot(100*p.variance.explained, las=2, xlab='', ylab='% Variance Explained')

#
#*****************************************************************
# 2-D Plot
#******************************************************************         
x = loadings[,1]
y = loadings[,2]
z = loadings[,3]
cols = as.double(sectors)

# plot all companies loadings on the first and second principal components and highlight points according to the sector they belong
plot(x, y, type='p', pch=20, col=cols, xlab='Comp.1', ylab='Comp.2')
text(x, y, data$symbolnames, col=cols, cex=.8, pos=4)

legend('topright', cex=.8,  legend = levels(sectors), fill = 1:nlevels(sectors), merge = F, bty = 'n') 

#*****************************************************************
# 3-D Plot, for good examples of 3D plots
# http://statmethods.wordpress.com/2012/01/30/getting-fancy-with-3-d-scatterplots/
#******************************************************************                 
load.packages('scatterplot3d') 

# plot all companies loadings on the first, second, and third principal components and highlight points according to the sector they belong
s3d = scatterplot3d(x, y, z, xlab='Comp.1', ylab='Comp.2', zlab='Comp.3', color=cols, pch = 20)

s3d.coords = s3d$xyz.convert(x, y, z)
text(s3d.coords$x, s3d.coords$y, labels=data$symbolnames, col=cols, cex=.8, pos=4)

legend('topleft', cex=.8,  legend = levels(sectors), fill = 1:nlevels(sectors), merge = F, bty = 'n') 
