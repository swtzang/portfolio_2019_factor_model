######################################################
# https://palomar.home.ece.ust.hk/MAFS6010R_lectures/Rsession_factor_models.html#final_comparison_of_covariance_matrix_estimations_via_different_factor_models
######################################################
rm(list = ls())
library(xts)
library(quantmod)

# set begin-end date and stock namelist
begin_date <- "2013-01-01"
end_date <- "2017-08-31"
stock_namelist <- c("AAPL", "AMD", "ADI",  "ABBV", "AEZS", "A",  "APD", "AA","CF")

# download data from YahooFinance
data_set <- xts()
for (stock_index in 1:length(stock_namelist))
  data_set <- cbind(data_set, Ad(getSymbols(stock_namelist[stock_index], 
                                            from = begin_date, to = end_date, auto.assign = FALSE)))
colnames(data_set) <- stock_namelist
tclass(data_set) <- "Date"

# download Fama-French factors from website
# url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
# temp <- tempfile()
# download.file(url, temp, method = "libcurl", mode = "wb")
# unzip(temp, "F-F_Research_Data_Factors_daily.CSV")
# unlink(temp)
mydata <- read.csv("F-F_Research_Data_Factors_daily.CSV", skip = 4)
mydata <- mydata[-nrow(mydata), ]  # remove last row
fama_lib <- xts(x = mydata[, c(2,3,4)], order.by = as.Date(paste(mydata[, 1]), "%Y%m%d"))
str(fama_lib)
#> An 'xts' object on 1926-07-01/2017-11-30 containing:
#>   Data: num [1:24120, 1:3] 0.1 0.45 0.17 0.09 0.21 -0.71 0.62 0.04 0.48 0.04 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : NULL
#>   ..$ : chr [1:3] "Mkt.RF" "SMB" "HML"
#>   Indexed by objects of class: [Date] TZ: UTC
#>   xts Attributes:  
#>  NULL
head(fama_lib)
#>            Mkt.RF   SMB   HML
#> 1926-07-01   0.10 -0.24 -0.28
#> 1926-07-02   0.45 -0.32 -0.08
#> 1926-07-06   0.17  0.27 -0.35
#> 1926-07-07   0.09 -0.59  0.03
#> 1926-07-08   0.21 -0.36  0.15
#> 1926-07-09  -0.71  0.44  0.56
tail(fama_lib)
#>            Mkt.RF   SMB   HML
#> 2017-11-22  -0.05  0.10 -0.04
#> 2017-11-24   0.21  0.02 -0.44
#> 2017-11-27  -0.06 -0.36  0.03
#> 2017-11-28   1.06  0.38  0.84
#> 2017-11-29   0.02  0.04  1.45
#> 2017-11-30   0.82 -0.56 -0.50

# compute the log-returns of the stocks and the Fama-French factors
X <- diff(log(data_set), na.pad = FALSE)
N <- ncol(X)  # number of stocks
T <- nrow(X)  # number of days
F <- fama_lib[index(as.zoo(X))]/100
#
F_ <- cbind(ones = 1, F)
Gamma <- t(solve(t(F_) %*% F_, t(F_) %*% X))
colnames(Gamma) <- c("alpha", "b1", "b2", "b3")
alpha <- Gamma[, 1]
B <- Gamma[, 2:4]
print(Gamma)

# Alternatively, we can simply use the R package covFactorModel to do the work for us:
# devtools::install_github("dppalomar/covFactorModel")  
library(covFactorModel)  
factor_model <- factorModel(X, type = "M", econ_fact = F) # type = M for Macro
cbind(alpha = factor_model$alpha, beta = factor_model$beta)









