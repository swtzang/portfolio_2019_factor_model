# https://systematicinvestor.wordpress.com/2012/06/20/factor-attribution/
###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
# Download Fama French factor data
# https://www.codingfinance.com/post/2018-05-31-download-ff-data-in-r/
###############################################################################
#setInternet2(TRUE)
rm(list = ls())
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
library(pacman)
p_load(quantmod, zoo, xts, tibble)   
tickers = 'VISVX'

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
dim(data$VISVX)
# tibble::rownames_to_column(data$VISVX, "date")

i = "VISVX"
for(i in ls(data)) {
  temp = adjustOHLC(data[[i]], use.Adjusted=T)                            
  
  period.ends = endpoints(temp, 'months')
  period.ends = period.ends[period.ends > 0]
  temp.01 = as_tibble(fortify.zoo(temp))
  # reformat date to match Fama French Data
  monthly.dates = as.Date(paste(format(temp.01$Index[period.ends], '%Y%m'),'01',sep=''), '%Y%m%d')
  data[[i]] = make.xts(coredata(temp[period.ends,]), monthly.dates)
}

# Fama/French factors
# factors = get.fama.french.data('F-F_Research_Data_Factors', periodicity = 'months',download = T, clean = F)
ff_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"
#
library(tidyverse)
# Create temp_file to store the file
temp_file <- tempfile()
# Download the file
download.file(ff_url, temp_file)
# Unzip the file, to extract the data
ff_factors_raw_data <- unzip(temp_file)
# Read the contents using tidyverse package
ff_factors_raw_data <- read_csv(ff_factors_raw_data)
# Check the data
head(ff_factors_raw_data)
# We need to load the file again
ff_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"

temp_file <- tempfile()
download.file(ff_url, temp_file)

ff_factors_raw_data <- unzip(temp_file)

# Skipping the first 3 rows

ff_factors_raw_data <- read_csv(ff_factors_raw_data, skip = 3)
head(ff_factors_raw_data)
#-------------------------------------
# add factors and align
data$factors = factors$data / 100
bt.prep(data, align='remove.na', dates='1994::')


#*****************************************************************
# Facto Loadings Regression over whole period
#****************************************************************** 
prices = data$prices
nperiods = nrow(prices)
dates = index(data$prices)

# compute simple returns    
hist.returns = ROC(prices[,tickers], type = 'discrete')
hist.returns = hist.returns - data$factors$RF
colnames(hist.returns) = 'fund'
hist.returns = cbind(hist.returns, data$factors$Mkt.RF,
                     data$factors$SMB, data$factors$HML)

fit.all = summary(lm(fund~Mkt.RF+SMB+HML, data=hist.returns))
estimate.all = c(fit.all$coefficients[,'Estimate'], fit.all$r.squared)
std.error.all = c(fit.all$coefficients[,'Std. Error'], NA)

#*****************************************************************
# Factor Loadings Regression over 36 Month window
#******************************************************************                             
window.len = 36

colnames = spl('alpha,MKT,SMB,HML,R2')
estimate = make.xts(matrix(NA, nr = nperiods, len(colnames)), dates)
colnames(estimate) = colnames
std.error = estimate

# main loop
for( i in window.len:nperiods ) {
  window.index = (i - window.len + 1) : i
  
  fit = summary(lm(fund~Mkt.RF+SMB+HML, data=hist.returns[window.index,]))
  estimate[i,] = c(fit$coefficients[,'Estimate'], fit$r.squared)
  std.error[i,] = c(fit$coefficients[,'Std. Error'], NA)
  
  if( i %% 10 == 0) cat(i, '\n')
}

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












