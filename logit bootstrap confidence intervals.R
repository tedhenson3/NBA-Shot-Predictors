#pca
setwd("~/shots")
library(tidyverse)

shots <- read.csv(file = "shots.csv", header = TRUE, stringsAsFactors = F, fill = T)
# shots$r <- sqrt(shots$shot_x^2 + shots$shot_y^2)
# 
# #22 feet to corner, 23.75 feet on arc
# ll <- sqrt(23.75^2 - 22^2)
# shots$is.three <- ((shots$shot_y < ll) & (shots$shot_x > 22)) | (shots$r >= 23.75)
# 
# threepointpct <- mean(shots$made[shots$is.three == T])
# threepointpct
# twopointfgpct <- mean(shots$made[shots$is.three == F])
# twopointfgpct


shots$made <- as.factor(shots$made)



set.seed(101)
split <- sample(nrow(shots), 7000)
# 
train <- shots[split,]
test <- shots[-split,]
library(boot)  

logit.bootstrap <- function(data, indices) {
  
  d <- data[indices, ]
  fit <- glm(made ~ . , data = d, family = 'binomial')
  please <- predict(fit, newdata = test, type = 'response', interval = "confidence")
  return(please)
}

set.seed(12345) 
logit.boot <- boot(data=train,  statistic = logit.bootstrap, R=1000) # 10'000 samples


plot(logit.boot)
ted <- boot.ci(logit.boot, conf = .95, type = "bca")



p <- data.frame(made  = c(0))
j <- 0
for(i in logit.boot$t0){
  j <- j + 1
  if(i > .5){
    p[j, 'made'] <- 1
  }
  else{

    p[j, 'made'] <- 0
  }
}
table(test[,1], p$made)
mean(test[,'made'] == p$made)