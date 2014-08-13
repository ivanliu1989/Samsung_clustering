setwd("C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Samsung_clustering")

## Unzip and load dataset into R
clusterEx.zip <- "clusteringEx_data.zip"
clusterEx.list <- unzip(clusterEx.zip)
clusterEx.list
faceData <- load(clusterEx.list[1])
head(faceData)
samsungData <- load(clusterEx.list[2])
head(samsungData)
