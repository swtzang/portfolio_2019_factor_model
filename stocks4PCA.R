rm(list = ls())
library(quantmod)
library(readxl)

stocks <- read_xlsx('stocks4PCA.xlsx')
head(stocks)
dim(stocks)

stocks.fit = princomp(stocks, cor = T) # performing on correlation matrix
stocks.fit$sdev # gives root_eigen_value
stocks.fit$sdev^2
loadings(stocks.fit)
#
stocks.fit.cov = princomp(stocks) # performing on covariance matrix
stocks.fit.cov$sdev # gives root_eigen_value
stocks.fit.cov$sdev^2
loadings(stocks.fit.cov)

#===============================================================
# devtools::install_github("dppalomar/covFactorModel", force = T)
library(covFactorModel)
#factor_model <- factorModel(retdata1, type = "S", K = K, max_iter = 10)
#cbind(alpha = factor_model$alpha, beta = factor_model$beta)
# Statistical 3-factor model
K <- 5
X_trn <- as.matrix(stocks)
T_trn <- dim(stocks)[1]
N <- dim(stocks)[2]
alpha <- colMeans(X_trn)
X_trn_ <- X_trn - matrix(alpha, T_trn, N, byrow = TRUE)
Sigma_prev <- matrix(0, N, N)
Sigma <- (1/(T_trn-1)) * t(X_trn_) %*% X_trn_
eigSigma <- eigen(Sigma)
while (norm(Sigma - Sigma_prev, "F")/norm(Sigma, "F") > 1e-3) {
  B <- eigSigma$vectors[, 1:K] %*% diag(sqrt(eigSigma$values[1:K]), K, K)
  Psi <- diag(diag(Sigma - B %*% t(B)))
  Sigma_prev <- Sigma
  Sigma <- B %*% t(B) + Psi
  eigSigma <- eigen(Sigma - Psi)
}
Sigma_PCA3 <- Sigma
B
eigSigma
#
library(factorAnalytics)
data(StockReturns)
class(r.M)
fit.pca <- fitSfm(r.M, k = 3)
names(fit.pca)

