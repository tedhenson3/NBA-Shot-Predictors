#pca
setwd("~/shots")

shots <- read.csv(file = "shotscopy.csv", header = TRUE, stringsAsFactors = F, fill = T)

shots$r <- sqrt(shots$shot_x^2 + shots$shot_y^2)

#22 feet to corner, 23.75 feet on arc
ll <- sqrt(23.75^2 - 22^2)
shots$is.three <- ((shots$shot_y < ll) & (shots$shot_x > 22)) | (shots$r >= 23.75)

threepointpct <- mean(shots$made[shots$is.three == T])
threepointpct
twopointfgpct <- mean(shots$made[shots$is.three == F])
twopointfgpct


shots <- read.csv(file = "shotscopy.csv", header = TRUE, stringsAsFactors = F, fill = T)

madeit <- shots$made
scaled.shots <- as.data.frame(scale(shots[,2:length(shots)]))

shots <- cbind(madeit, scaled.shots)

library(caTools)

set.seed(101)
split <- sample(nrow(shots), 6000)

train <- shots[split,]
test <- shots[-split,]
pca <- prcomp(train)



covariancematrix <- cov(pca$x)


eigenmatrix <- diag(pca$sdev^2)

eigenmatrix <- eigenmatrix[1:9, 1:6]

shots.eigen <- shots*eigenmatrix


x <- shots.eigen
wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
  km.out <- kmeans(x, centers = i, nstart = 20, iter.max = 1000)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

shots <- x

hclust.complete <- hclust(d = dist(shots), method = 'complete')
hclust.single <- hclust(d = dist(shots), method = 'single')
hclust.average <- hclust(d = dist(shots), method = 'average')


whoknows <- cutree(hclust.complete, k = 2)
single <- cutree(hclust.single, k = 2)
average <- cutree(hclust.average, k = 2)



shots$complete.cluster <- whoknows
shots$single.cluster <- single
shots$average.cluster <- average


library(tidyverse)

complete <- shots %>% group_by(complete.cluster) %>% summarise(numberinsection = n(), fgpct = mean(madeit))
complete

single <- shots %>% group_by(single.cluster) %>% summarise(numberinsection = n(), fgpct = mean(madeit))
single

average <- shots %>% group_by(average.cluster) %>% summarise(numberinsection = n(), fgpct = mean(madeit))
average



# 
# 
# # Variability of each principal component: pr.var
# pr.var <- x.pca$sdev^2
# 
# # Variance explained by each principal component: pve
# pve <- pr.var / sum(pr.var)
# 
# 
# 
# # Plot variance explained for each principal component
# plot(x = pve, xlab = "Principal Component",
#      ylab = "Proportion of Variance Explained",
#      ylim = c(0, 1), type = "b")
# 
# # Plot cumulative proportion of variance explained
# plot(x = cumsum(pve),  xlab = "Principal Component",
#      ylab = "Cumulative Proportion of Variance Explained",
#      ylim = c(0, 1), type = "b")
# 
# 
# ted <- predict(x.pca, newdata = test)
# 
# error <- test$madeit - ted
# 
# svm_error <- sqrt(mean(error^2))
# 
