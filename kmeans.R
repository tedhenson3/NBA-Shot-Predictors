setwd("C:/Users/tedhe/Onedrive/Documents/shots")

shots <- read.csv(file = "shotscopy.csv", header = TRUE, stringsAsFactors = F, fill = T)

##I put the made column as the first row of the csv in excel####


end <- length(shots)
maxs <- apply(shots[,2:end], 2, max)
mins <- apply(shots[,2:end], 2, min)

scaled.data <- as.data.frame(scale(shots[,2:end],center = mins, scale = maxs - mins))

made = shots$made
data =  cbind(made, scaled.data)
shots <- data
shots <- shots[,1:3]
wss <- (nrow(shots)-1)*sum(apply(shots,2,var))
for (i in 2:15){
wss[i] <- sum(kmeans(shots, centers=i)$withinss)
}
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
fit <- kmeans(x = shots, centers = 3)

library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)



d <- dist(shots, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2") 
plot(fit) 
groups <- cutree(fit, k=3) # cut tree into 5 clusters
rect.hclust(fit, k=3, border="red")

library(pvclust)
tshots <- t(shots)
fit <- pvclust(tshots, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)
# get cluster means 
