setwd("C:/Users/tedhe/Onedrive/Documents/shots")

shots <- read.csv(file = "shotscopy.csv", header = TRUE)

shots$xsection <- cut(shots$shot_x, c(min(shots$shot_x), -10, 0, 10, max(shots$shot_x)))
shots$ysection <- cut(shots$shot_y, c(min(shots$shot_y), 10, 25, 40, max(shots$shot_x)))

library(tidyverse)
data <- shots %>% group_by(xsection, ysection) %>% summarise(fgpct = mean(made))
data

shots <- data


svmmodel <- svm(fgpct~ysection + xsection, shots)

nrow(shots)

##I put the made column as the first row of the csv in excel####


library(e1071)

#Fit a model. The function syntax is very similar to lm function


feats <- names(shots[2:length(shots)])
feats

f <- paste(feats,collapse=' + ')
f <- paste('made ~',f)

f <- as.formula(f)
model_svm <- svm(madeit ~ ., train)



#Use the predictions on the data

pred <- predict(model_svm, x)

error_2 <- x$made - pred


error <- model$residuals

lm_error <- sqrt(mean(error^2)) # 3.832974


svm_error <- sqrt(mean(error_2^2))


svm_error
lm_error

#Plot the predictions and the plot to see our model fit
plot.new()
points(shots$made, pred, col = "blue", pch=4)
