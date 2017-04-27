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
inner <- subset(inner, (pricevar < 170000))
clean <- subset(inner, select = -c(pricevar, amenities, 
                                   amenities_list, 
                                   requires_license, host_thumbnail_url,
                                   host_picture_url, picture_url))
nums <- sapply(clean, is.numeric)
clean_num <- clean[,nums]
clean_num <- na.omit(clean_num)

linearmodel <- lm(avgprice~., data = clean_num)
features <- subset(linearmodeldf, p.value < 0.1)
featurenames <- features$term
feat_new <- clean_num[c(featurenames[2:29], "avgprice")]
feat_new <- subset(feat_new, avgprice < 600)

n = dim(feat_new)[1] ### total number of observations
n1 = round(n/5) ### number of obs randomly selected for testing data

B = 100
TEALL = NULL

for(b in 1:B) {
  flag = sort(sample(1:n, n1));
  train = feat_new[-flag,]; ### training set
  test = feat_new[flag,]; ### testing set
  
  #lm
  ytrue = test$avgprice
  linearmodel <- lm(avgprice~., data = train)
  #linearmodeldf <- tidy(linearmodel)
  pred1a <- predict(linearmodel, test[,-29]);
  MSETestmod1 <-   mean((pred1a - ytrue)^2); 
  
  # stepwise
  stepmodel <- stepAIC(linearmodel, direction="both")
  pred2a <- predict(stepmodel, test[,-93]);
  MSETestmod2 <-   mean((pred2a - ytrue)^2); 
  
  # lasso
  lassomodel <- glmnet(clean[c(1:106,108:118)],clean$price_num)
  fit3 <- predict(lassomodel, as.matrix(test[,-100]), s=lasso.lambda, type="fit", mode="lambda");
  yhat3 <- fit5b$fit; 
  MSETestmod3 <- mean( (yhat5b - test$avgprice)^2); 
  
  TEALL = rbind(TEALL, cbind(MSETestmod1, MSETestmod2, MSETestmod3));
}  

  # ridge
  ridgemodel <- lm.ridge(clean$avgprice~., data = clean)
  
  # knn regression
  knn_clean = na.omit(clean)
  ## can only use numeric columns
  nums <- sapply(knn_clean, is.numeric)
  knnmodel <- knn.reg(knn_clean[,nums], y=knn_clean$avgprice, k=3)

}