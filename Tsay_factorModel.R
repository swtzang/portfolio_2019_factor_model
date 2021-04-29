# PCA example : Tsay, Time series analysis
# Table 9.3 
rtn=read.table('m-5clog-9008.txt', header = TRUE)
str(rtn)
cov(rtn)
cor(rtn)

pca.cov = princomp(rtn)
pca.cov$sdev^2 # this gives eigenvalues based on cov matrix
names(pca.cov)
summary(pca.cov)
pca.cov$loadings
screeplot(pca.cov)
pca.corr=princomp(rtn, cor=T)
pca.corr$sdev^2 # this gives eigenvalues based on corr. matrix



