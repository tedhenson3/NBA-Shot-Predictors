setwd("C:/Users/tedhe/Onedrive/Documents/shots")

shots <- read.csv(file = "shotscopy.csv", header = TRUE, stringsAsFactors = F, fill = T)

##I put the made column as the first row of the csv in excel####

colnames(shots)

shots <- abs(shots)
