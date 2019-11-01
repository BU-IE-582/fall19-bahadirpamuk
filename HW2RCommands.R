library(data.table)
library(dplyr)
library(tidyr)
library("plot3D")
library(corrplot)


MuskData <- fread(file="c:/Users/BAHADIR/Desktop/IE 582/HW 2/RCode/Musk1.csv", header=FALSE, sep="," , stringsAsFactors=TRUE)
MuskDataReduced <- MuskData[,3:168]

pca<-princomp(MuskDataReduced,cor=T)
par(mfrow=c(1,1))

plot(pca$scores[,1], pca$scores[,2], col=(MuskData[,V1]+1), pch=".",cex=7)
plot(pca$scores[,2], pca$scores[,3], col=(MuskData[,V1]+1), pch=".",cex=7)
plot(pca$scores[,1], pca$scores[,3], col=(MuskData[,V1]+1), pch=".",cex=7)
scatter3D(pca$scores[,1], pca$scores[,2], pca$scores[,3], col=(MuskData[,V1]+1), pch=".",cex=5, theta = 40, phi = -5)


distance <- dist(MuskDataReduced, method = "manhattan", diag = TRUE, upper = TRUE)
mds=cmdscale(distance)
plot(mds[,1],mds[,2],main='Manhattan', col=(MuskData[,V1]+1), pch=".",cex=7)

distance <- dist(MuskDataReduced, method = "minkowski", diag = TRUE, upper = TRUE , p=1.5)
mds=cmdscale(distance)
plot(mds[,1],mds[,2],main='Minkowski p=1.5', col=(MuskData[,V1]+1), pch=".",cex=7)

distance <- dist(MuskDataReduced, method = "euclidean", diag = TRUE, upper = TRUE)
mds=cmdscale(distance)
plot(mds[,1],mds[,2],main='Euclidean', col=(MuskData[,V1]+1), pch=".",cex=7)

distance <- dist(MuskDataReduced, method = "minkowski", diag = TRUE, upper = TRUE , p=3)
mds=cmdscale(distance)
plot(mds[,1],mds[,2],main='Minkowski p=3', col=(MuskData[,V1]+1), pch=".",cex=7)

distance <- dist(MuskDataReduced, method = "maximum", diag = TRUE, upper = TRUE)
mds=cmdscale(distance)
plot(mds[,1],mds[,2],main='Maximum', col=(MuskData[,V1]+1), pch=".",cex=7)

MuskDataCombined <- as.data.frame(MuskData[, lapply(.SD, mean),by = V2])
MuskDataCombinedReduced <- MuskDataCombined[,3:168]


corrplot(cor(MuskDataCombinedReduced), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrMatrix <- cor(MuskDataCombinedReduced)
corrMatrix[upper.tri(corrMatrix)] <- 0
diag(corrMatrix) <- 0

data.new <- MuskDataCombinedReduced[,!apply(corrMatrix,2,function(x) any(abs(x) > 0.95))]

pca<-princomp(data.new,cor=T)

plot(pca$scores[,1], pca$scores[,2], col=(MuskDataCombined[,'V1']+1), pch=".",cex=7)
plot(pca$scores[,2], pca$scores[,3], col=(MuskDataCombined[,'V1']+1), pch=".",cex=7)
plot(pca$scores[,1], pca$scores[,3], col=(MuskDataCombined[,'V1']+1), pch=".",cex=7)
scatter3D(pca$scores[,1], pca$scores[,2], pca$scores[,3], col=(MuskDataCombined[,'V1']+1), pch=".",cex=5, theta = 40, phi = -5)

distance <- dist(MuskDataCombinedReduced, method = "manhattan", diag = TRUE, upper = TRUE)
mds=cmdscale(distance)
plot(mds[,1],mds[,2],main='Manhattan', col=(MuskDataCombined[,'V1']+1), pch=".",cex=7)

distance <- dist(MuskDataCombinedReduced, method = "minkowski", diag = TRUE, upper = TRUE , p=1.5)
mds=cmdscale(distance)
plot(mds[,1],mds[,2],main='Minkowski p=1.5', col=(MuskDataCombined[,'V1']+1), pch=".",cex=7)

distance <- dist(MuskDataCombinedReduced, method = "euclidean", diag = TRUE, upper = TRUE)
mds=cmdscale(distance)
plot(mds[,1],mds[,2],main='Euclidean', col=(MuskDataCombined[,'V1']+1), pch=".",cex=7)

distance <- dist(MuskDataCombinedReduced, method = "minkowski", diag = TRUE, upper = TRUE , p=3)
mds=cmdscale(distance)
plot(mds[,1],mds[,2],main='Minkowski p=3', col=(MuskDataCombined[,'V1']+1), pch=".",cex=7)

distance <- dist(MuskDataCombinedReduced, method = "maximum", diag = TRUE, upper = TRUE)
mds=cmdscale(distance)
plot(mds[,1],mds[,2],main='Maximum', col=(MuskDataCombined[,'V1']+1), pch=".",cex=7)

library(imager)
resim <- load.image("c:/Users/BAHADIR/Desktop/IE 582/HW 2/Picture.jpg")
str(resim)
par(mfrow=c(1,1))
plot(resim)

RNoise <- replicate(256,runif(256,min(resim[,,1]),0.1*max(resim[,,1])))
GNoise <- replicate(256,runif(256,min(resim[,,2]),0.1*max(resim[,,2])))
BNoise <- replicate(256,runif(256,min(resim[,,3]),0.1*max(resim[,,3])))

noisyImage <- resim
noisyImage[,,1] <- (noisyImage[,,1] + RNoise)
noisyImage[,,2] <- (noisyImage[,,2] + GNoise)
noisyImage[,,3] <- (noisyImage[,,3] + BNoise)

noisyImage[,,1] <- ifelse(noisyImage[,,1] >1 , 1, noisyImage[,,1])
noisyImage[,,2] <- ifelse(noisyImage[,,2] >1 , 1, noisyImage[,,2])
noisyImage[,,3] <- ifelse(noisyImage[,,3] >1 , 1, noisyImage[,,3])

plot(noisyImage)

par(mfrow=c(1,3))

cscale <- function(r,g,b) rgb(r,0,0)
plot(noisyImage,colourscale=cscale,rescale=FALSE)
cscale <- function(r,g,b) rgb(0,g,0)
plot(noisyImage,colourscale=cscale,rescale=FALSE)
cscale <- function(r,g,b) rgb(0,0,b)
plot(noisyImage,colourscale=cscale,rescale=FALSE)

par(mfrow=c(1,1))
grayNoisyImage <- grayscale(noisyImage)
plot(grayNoisyImage)

patchesCoordX <- rep(seq(13,244,1),232)
patchesCoordY <- rep(seq(13,244,1),each = 232)
patches <- extract_patches(grayNoisyImage, patchesCoordX, patchesCoordY, 25, 25, boundary_conditions = 0L)
dataFrames <- as.data.frame(matrix(unlist(patches), nrow=length(patches), byrow=T))
dim(dataFrames)

pca<-princomp(dataFrames,cor=T)
plot(pca$scores[,1], pca$scores[,2], pch=".",cex=1)
plot(pca$scores[,2], pca$scores[,3], pch=".",cex=1)
plot(pca$scores[,1], pca$scores[,3], pch=".",cex=1)
scatter3D(pca$scores[,1], pca$scores[,2], pca$scores[,3],pch=".",cex=1, theta = 55, phi = -5)

pca1Pic <- matrix(pca$scores[,1],nrow=232,ncol =232)
dim(pca1Pic)
plot(as.cimg(pca1Pic))

pca2Pic <- matrix(pca$scores[,2],nrow=232,ncol =232)
plot(as.cimg(pca2Pic))

pca3Pic <- matrix(pca$scores[,3],nrow=232,ncol =232)
plot(as.cimg(pca3Pic))

pca1Pic <- matrix(pca$loadings[,1],nrow=232,ncol =232)
dim(pca1Pic)
plot(as.cimg(pca1Pic))

pca2Pic <- matrix(pca$loadings[,2],nrow=232,ncol =232)
plot(as.cimg(pca2Pic))

pca3Pic <- matrix(pca$loadings[,3],nrow=232,ncol =232)
plot(as.cimg(pca3Pic))