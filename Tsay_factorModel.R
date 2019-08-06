# PCA example : Tsay, Time series analysis
#
rtn=read.table('m-5clog-9008.txt', header = TRUE)

str(rtn)
pca.cov = princomp(rtn)
names(pca.cov)
summary(pca.cov)
pca.cov$loadings
screeplot(pca.cov)
pca.corr=princomp(rtn,cor=T)
summary(pca.corr)



