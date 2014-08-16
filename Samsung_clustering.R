setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Samsung_clustering")

## Unzip and load dataset into R
clusterEx.zip <- "clusteringEx_data.zip"
clusterEx.list <- unzip(clusterEx.zip)
clusterEx.list
load(clusterEx.list[2])
names(samsungData)[1:12]
table(samsungData$activity)

## Plotting average acceleration for first subject
png("AvgAcc.png",height = 480,width = 960)
par(mfcol = c(1,2),mar = c(5,4,1,1))
samsungData <- transform(samsungData, activity=factor(activity))
sub1 <- subset(samsungData,subject =1 )
plot(sub1[,1], col=sub1$activity, ylab=names(sub1)[1])
plot(sub1[,2], col=sub1$activity, ylab=names(sub1)[2])
legend("bottomright", legend=unique(sub1$activity), col=unique(sub1$activity), pch=1)
dev.off()

## Clustering based just on average acceleration
source("myplclust.R")
distanceMatrix <- dist(sub1[,1:3])
hclustering  <- hclust(distanceMatrix)
png("AvgAccDist.png",height = 480,width = 960)
myplclust(hclustering, lab.col = unclass(sub1$activity))
dev.off()

## Plotting max acceleration for the first subject
png("MaxAcc1.png", height = 480,width = 960)
par(mfrow = c(1,2))
plot(sub1[,10], pch=19, col = sub1$activity, ylab = names(sub1)[10])
plot(sub1[,11], pch=19, col = sub1$activity, ylab = names(sub1)[11])
legend("bottomright", pch=1, legend = unique(sub1$activity), col = unique(sub1$activity))
dev.off()

## Clustering based on maximum acceleration
source("myplclust.R")
distanceMatrix <- dist(sub1[,10:12])
hclustering  <- hclust(distanceMatrix)
png("MaxAccDist.png",height = 480,width = 960)
myplclust(hclustering, lab.col = unclass(sub1$activity))
dev.off()

## Singular value decomposition
svd1 = svd(scale(sub1[,-c(562,563)]))
png("SVD.png", height = 480,width = 960)
par(mfrow = c(1,2))
plot(svd1$u[,1], pch=19, col = sub1$activity)
plot(svd1$u[,2], pch=19, col = sub1$activity)
legend("bottomright", pch=1, legend = unique(sub1$activity), col = unique(sub1$activity))
dev.off()