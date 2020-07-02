#pca
setwd("~/shots")
library(tidyverse)

shots <- read.csv(file = "shots.csv", header = TRUE, stringsAsFactors = F, fill = T)
shots$r <- sqrt(shots$shot_x^2 + shots$shot_y^2)

#22 feet to corner, 23.75 feet on arc
ll <- sqrt(23.75^2 - 22^2)
#shots$is.three <- ((shots$shot_y < ll) & (shots$shot_x > 22)) | (shots$r >= 23.75)



side <- function(x) {
  if (x > 0) {
    result <- "Right"
  }
  else if (x < 0) {
    result <- "Left"
  }
  else {
    result <- "Zero"
  }
  return(result)
}
# shots$courtside <- sapply(shots$shot_x, FUN = side)
# 
# shots$defangleside <- sapply(shots$defender_velocity_angle, FUN = side)
# shots$defangle <- sapply(shots$defender_velocity_angle, FUN = side)
# 
# shots$shootangleside <- sapply(shots$shooter_velocity_angle, FUN = side)
# 
# shots$shootangleside <- factor(shots$shootangleside)
# shots$defangle <- factor(shots$defangle)
# shots$defangleside <- factor(shots$defangleside)
# shots$courtside <- factor(shots$courtside)
# 

set.seed(1991)

shots <- shots[, -c(2,3)]

split <- sample(nrow(shots[,2:c(ncol(shots))]), 8000)
train <- shots[split,]
test <- shots[-split,]
library(boot)  
library(glmnet)

library(bestglm)
library(MASS)


fit <- glmnet(x = as.matrix(train[,2:ncol(train)]), y = as.vector(train$made) ,  family = 'binomial', alpha = .5)


xy <- cbind(train[,2:ncol(train)], train$made)
best <- bestglm(xy, IC = 'AIC')
summary(best)

# library(randomForest)
# rf <- randomForest(made ~ ., data = train)
#step <- stepAIC(fit, direction = "both")
# 
# RESULT=NULL
# for (i in 0:100) {
#   cv.out = cv.glmnet(x=as.matrix(train[,2:ncol(train)]),
#                      y=as.vector(train$made),
#                      type.measure="mse", 
#                      alpha=i/100)
#   alpha=i/100
#   best.lambda=cv.out$lambda.1se
#   y.test=predict(cv.out,s=best.lambda,newx=as.matrix(test[,2:ncol(test)]))
#   out.mse=mean((as.vector(test$made)-y.test)^2)
#   RESULT=rbind(RESULT,c(alpha,best.lambda,out.mse))
# }
# colnames(RESULT)=c("alpha","lambda","MSE")
# RESULT <- as.data.frame(RESULT)
# print(RESULT)
# 
# best <- RESULT[which.min(RESULT$MSE), 'MSE']
# best
# please <- predict(fit, newx = as.matrix(test[,2:ncol(test)]), type = 'response', interval = 'confidence')
# 
# median(please[,3] - please[,2])
# mean(please[,3] - please[,2])

please <- predict(best$BestModel, newdata = test, type = 'response', interval = 'confidence')
median(please[,3] - please[,2])
mean(please[,3] - please[,2])
#find how many intervals that contain .5, filter###



library(tidyverse)
please <- as.data.frame(please)
please <- please[-c(which(please$lwr < .5 & please$upr > .5)),]


p <- data.frame(made  = c(0))
j <- 0

test <- test[-c(which(please$lwr < .5 & please$upr > .5)),]
for(i in 1:length(please)){
  if(please[i] > .7){
    p[i, 'made'] <- 1
  }
  else{
    
    p[i, 'made'] <- 0
  }
}
table(test[,1], p$made)
mean(test[,'made'] == p$made)

# 
# 
# step
# 
# ctrl <- train
# 
# ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
# 
# mod_fit <- train(Class ~ Age + ForeignWorker + Property.RealEstate + Housing.Own + 
#                    CreditHistory.Critical,  data=GermanCredit, method="glm", family="binomial",
#                  trControl = ctrl, tuneLength = 5)
# 
# pred = predict(mod_fit, newdata=testing)
# confusionMatrix(data=pred, testing$Class)
# 
# 
# logit.bootstrap <- function(data, indices) {
#   
#   d <- data[indices, ]
#   fit <- glm(made ~ . , data = d, family = 'binomial')
#   step.model <- fit %>% stepAIC(trace = FALSE)
#   please <- predict(step.model, newdata = test, type = 'response')
#   return(please)
# }
# 
# set.seed(12345) 
# logit.boot <- boot(data=train,  statistic = logit.bootstrap, R=1000) # 10'000 samples
# 
# 
# 
# output <- logit.boot
# bias <- mean(output$t)-output$t0
# abbias <- abs(bias)
# summary(abbias)
# typicalbias <- median(abbias)
# #se: 
# se <- sd(output$t)
# 
# library(stringr)
# 
# ##lasso glm##
# 
# x <- capture.output(output) # store the output as text
# x <- str_extract(x ,"^t1.*$") # grab the line that starts with t1
# x <- x[!is.na(x)] # remove all the lines we don't need
# se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$'))) # extract the final value (se)
# #[1] 0.4119374
# 
# 
# 
# plot(logit.boot)
# library(ciTools)
# ted <- boot.ci(logit.boot, conf = .95, type = "all")
# 
# p <- data.frame(made  = c(0))
# j <- 0
# for(i in length(please)){
#   if(please[i] > .5){
#     p[i, 'made'] <- 1
#   }
#   else{
# 
#     p[i, 'made'] <- 0
#   }
# }
# table(test[,1], p$made)
# mean(test[,'made'] == p$made)
# 
# 
# library(caret)
# library(pRoc)
# library(ROCR)
# 
# 
# 
# fit <- glm(made ~ . , data = train, family = 'binomial')
# 
# library(pscl)
# pR2(fit)
# 
# library(MKmisc)
# HLgof.test(fit = fitted(fit), obs = train$made)
# library(ResourceSelection)
# hoslem.test(train$made, fitted(fit), g=10)
# 
# library(survey)
# regTermTest(fit)
# 
# 
# ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
# 
# mod_fit <- train(made ~ .,  data=train, method="glm", family="binomial",
#                  trControl = ctrl, tuneLength = 5)
# 
# pred = predict(step, newdata=test)
# confusionMatrix(data=pred, test$made)
# 
