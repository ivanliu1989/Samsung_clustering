setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Samsung_clustering")

## Unzip and load dataset into R
clusterEx.zip <- "clusteringEx_data.zip"
clusterEx.list <- unzip(clusterEx.zip)
clusterEx.list
load(clusterEx.list[2])
names(samsungData)[1:12]
table(samsungData$activity)

## Plotting average acceleration for first subject
par(mfcol = c(1,2),mar = c(5,4,1,1))
samsungData <- transform(samsungData, activity=factor(activity))
sub1 <- subset(samsungData,subject =1 )
plot(sub1[,1], col=sub1$activity, ylab=names(sub1)[1])
plot(sub1[,2], col=sub1$activity, ylab=names(sub1)[2])
legend("bottomright", legend=unique(sub1$activity), col=unique(sub1$activity), pch=1)
