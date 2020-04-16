rm(list=ls())
snp_data <- read.csv("https://raw.githubusercontent.com/linnylin92/469_public/master/data/snp_data.csv", row.names = 1)
heart_disease <- snp_data[,1]
snp_data <- as.matrix(snp_data[,-1])
dim(snp_data)
snp_data <- scale(snp_data)

# let's try using sparse PCA on the snp_data
# let's first review the usual PCA
## this is roughly what you did in HW3
pca_res <- stats::prcomp(snp_data, center = T, scale. = T)
plot(pca_res$x[,1], pca_res$x[,2], asp = T, pch = 16, col = as.numeric(as.factor(heart_disease)))

# now let's use sparse PCA
## first, let's use the cross validation to tune the parameter
set.seed(10)
## this function REQUIRES you to pass in a numeric matrix
class(snp_data)
spca_cv_res <- PMA::SPC.cv(snp_data, sumabsvs = seq(1.2, sqrt(ncol(snp_data))/2, length.out = 10))
## i.e., the following function will fail since it's not a numeric matrix
spca_cv_res <- PMA::SPC.cv(data.frame(snp_data), sumabsvs = seq(1.2, sqrt(ncol(snp_data))/2, length.out = 10))

spca_cv_res
plot(spca_cv_res)
