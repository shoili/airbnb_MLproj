library(broom)
library(dplyr)
library(MASS)
library(glmnet)
library(FNN)

# read data
inner <- read.csv("dat.csv")

# remove useless columns
inner$X <- NULL
inner <- subset(inner, select = -c(id))
clean <- subset(inner, select = -c(amenities, amenities_list))

#lm
linearmodel <- lm(clean$avgprice~., data = clean)
linearmodeldf <- tidy(linearmodel)

# stepwise
stepmodel <- stepAIC(linearmodel, direction="both")

# lasso
lassomodel <- glmnet(clean[c(1:106,108:118)],clean$price_num)

# ridge
ridgemodel <- lm.ridge(clean$avgprice~., data = clean)

# knn regression
knn_clean = na.omit(clean)
## can only use numeric columns
nums <- sapply(knn_clean, is.numeric)
knnmodel <- knn.reg(knn_clean[,nums], y=knn_clean$avgprice, k=3)
