# Multifactor Model ====
# 
#
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

#R-square ----
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
#as.vector(mvp.w, names = rownames(mvp.w))
barplot(as.vector(mvp.w), ylim = c(-0.01, 0.4), names.arg = rownames(mvp.w), cex.names = 0.5)
# ggplot2
library(ggplot2)
library(tidyverse)
#
tickers<-rownames(mvp.w)
weighti<-as.numeric(mvp.w)
mvp.w.df<-data.frame(tickers, weighti)
#
mvp.w.df %>% 
  ggplot(aes(tickers, weighti)) +
  geom_bar(stat = "identity")

# pca ----
# https://stackoverflow.com/questions/37338278/pca-how-does-princomp-work-and-can-i-use-it-to-pick-up-variables-for-arima
X <- retdata1
S <- cor(X)  ## get correlation matrix S
E <- eigen(S)  ## compute eigen decomposition of S
root_eigen_value <- sqrt(E$values)  ## square root of eigen values
root_eigen_value
eigen_vector_mat <- E$vectors  ## matrix of eigen vectors
eigen_vector_mat
X1 <- scale(X) %*% eigen_vector_mat  ## transform original matrix
X1

# continue to use Berndtdata
# use R princomp() function for principal component analysis
# pc.fit = princomp(retdata1, cor = TRUE)
#
pc.fit = princomp(retdata1)
class(pc.fit)[1] 
pc.fit
names(pc.fit)
# [1] "sdev"     "loadings" "center"   "scale"    "n.obs"    "scores"   "call" 
summary(pc.fit)
#
cov.pca <- diag(pc.fit$sdev^2)
#
plot(pc.fit)
pc.fit.per <- round(pc.fit$sdev^2 / sum(pc.fit$sdev^2)*100, 1)
loadings(pc.fit)
loadings(pc.fit)[,1] 
#Notice that all of the estimated loadings on the ﬁrst factor are positive

head(pc.fit$scores[, 1:4])
# The following use codes from : 
# https://www.youtube.com/watch?v=0Jp4gsfOLMs
# Note: prcomp() expects samples (stock id) to be rows and genes (stock returns)　to be columns.
# Since our data matrix stores stock returns by rows and stock id by columns, we need to transpose the matrix.
rownames(retdata1) <- paste("period", 1:120, sep="")
pca <- prcomp(t(retdata1), scale = T)
names(pca)
#[1] "sdev"     "rotation" "center"   "scale"    "x"
# x contains the principal components (PCs) 
# Since there are 15 stocks, ther are 15 PCs.
# The first PC accounts for the most variation in the original data (the returns across all 15 stocks).
# The 2nd PC accounts for the second most variation and so on. To plot a 2-D PCA graph, we usually use the 
# first 2 PCs. However, sometimes we use PC2 and PC3. 
pca$x
plot(pca$x[,1], pca$x[,2])
#
# We use the square of sdev, which stands for "standard deviation", to calculate how much 
# variation in the original data each principal component accounts for. 
pca.var <- pca$sdev^2
pca.var
# The percentage of variation that each PC accounts for
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main = "Scree Plot", xlab = "Principal Component", ylab = "Percent Variation")
#
library(ggplot2)
pca.data <- data.frame(stock = rownames(pca$x), 
                       X = pca$x[,1], 
                       Y = pca$x[,2])
pca.data
#
ggplot(data = pca.data, aes(x = X, y =Y, label = stock)) + 
  geom_text()+
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep = "")) + 
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep = "")) +
  theme_bw() +
  ggtitle("My PCA Graph")
#
loading_scores <- pca$rotation[,1]
return_scores <- abs(loading_scores)
return_score_ranked <- sort(return_scores, decreasing = TRUE)
top_20_returns <- names(return_score_ranked[1:20])



