

shots <- read.csv(file = "shots.csv", header = TRUE)

##I put the made column as the first row of the csv in excel####


end <- length(shots)
maxs <- apply(shots[,2:end], 2, max)
mins <- apply(shots[,2:end], 2, min)




scaled.data <- as.data.frame(scale(shots[,2:end],center = mins, scale = maxs - mins))


made = shots$made
data =  cbind(made,scaled.data)




print(head(data,2))

library(caTools)
library(class)
library(gmodels)


set.seed(101)

ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.75, 0.25))

train <- data[ind==1, 2:end]
test <- data[ind==2, 2:end]


trainLabels <- data[ind==1, 1]
testLabels <- data[ind==2, 1]





knn1 <- knn(train = train, test = test, cl = trainLabels, k = 7)
CrossTable(x = testLabels, y = knn1, prop.chisq=FALSE)





