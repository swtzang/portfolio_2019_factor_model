#========================================
# multifactor model
#========================================
rm(list=ls())
#setwd("D:/亞洲大學上課資料/Portfolio management 2016 Fall")
retdata = read.csv('berndt.csv')
t = dim(retdata)[1]
t
market = retdata[,10]

riskfree = retdata[,17]
market = market - riskfree

retdata1 = retdata[,c(-10, -17)]
retdata1 = as.matrix(retdata1)
n = dim(retdata1)[2]
n
riskfree_mtx = matrix(rep(riskfree,n), ncol=n)
retdata1<-retdata1 - riskfree_mtx
ones = rep(1,t)
X = cbind(ones,market)
b_hat = solve(t(X)%*%X)%*%t(X)%*%retdata1
E_hat = retdata1 - X%*%b_hat
diagD_hat = diag(t(E_hat)%*%E_hat)/(t-2)

#R-square
retvar = apply(retdata1,2,var) 
R_2 = 1 - diag(t(E_hat)%*%E_hat)/((t-1)*retvar)
res_std = sqrt(diagD_hat)
cov_factor = var(market)*t(b_hat)%*%b_hat + diag(diagD_hat) 
sd = sqrt(diag(cov_factor));
cor_factor = cov_factor/(sd%*%t(sd));
# sample variance and correlation matrix
cov_sample = cov(retdata1);
cor_sample = cor(retdata1);
# use factor covariance matrix to compute global minimum variance portfolio
one.vec = rep(1,15)
a = solve(cov_factor)%*%one.vec
b = t(one.vec)%*%a
mvp.w =a / as.numeric(b)
as.vector(mvp.w, names = rownames(mvp.w))
barplot(as.vector(mvp.w), ylim = c(-0.01, 0.4), names.arg = rownames(mvp.w), cex.names = 0.5)

