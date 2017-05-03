library(broom)
library(dplyr)
library(MASS)
library(glmnet)
library(FNN)
library(lars)

# read data
inner <- read.csv("dat.csv")

# remove useless columns
inner$X <- NULL
inner <- subset(inner, select = -c(id))
inner <- subset(inner, (pricevar < 100))
clean <- subset(inner, select = -c(pricevar, amenities, 
                                   amenities_list, 
                                   requires_license, host_thumbnail_url,
                                   host_picture_url, picture_url))
nums <- sapply(clean, is.numeric)
clean_num <- clean[,nums]
clean_num <- na.omit(clean_num)

######## set up for linear regression ###########
linearmodel <- lm(avgprice~., data = clean_num)
# remove features w/ pvalues > 0.1
linearmodeldf <- tidy(linearmodel)
features <- subset(linearmodeldf, p.value < 0.1)
featurenames <- features$term
feat_new <- clean_num[c(featurenames[2:29], "avgprice")]
# perform regressions on 2 subsets of data, price below 100 & price below 200
# listings above 200 have horrendous MSE
clean_100 <- subset(feat_new, avgprice < 100)
clean_200 <- subset(feat_new, avgprice >= 100 & avgprice < 200)
clean_above <- subset(feat_new, avgprice >= 200)

n_100 = dim(clean_100)[1] ### total number of observations
n1_100 = round(n_100/5) ### number of obs randomly selected for testing data
n_200 = dim(clean_200)[1] ### total number of observations
n1_200 = round(n_200/5) ### number of obs randomly selected for testing data

############# setup for lasso regression ###############
features_more <- subset(linearmodeldf, p.value < 0.3)
featurenames_more <- features_more$term
feat_new_more <- clean_num[c(featurenames_more[2:52], "avgprice")]
clean_100_more <- subset(feat_new_more, avgprice < 100)
clean_200_more <- subset(feat_new_more, avgprice >= 100 & avgprice < 200)

n_100_more = dim(clean_100_more)[1] ### total number of observations
n1_100_more = round(n_100_more/5) ### number of obs randomly selected for testing data
n_200_more = dim(clean_200_more)[1] ### total number of observations
n1_200_more = round(n_200_more/5) ### number of obs randomly selected for testing data

############ setup for ridge ############################
blah_100 <- clean_100_more[,which(colSums(abs(clean_100_more)) !=0)]
blah_200 <- clean_200_more[,which(colSums(abs(clean_200_more)) !=0)]
n_100_blah = dim(blah_100)[1] ### total number of observations
n1_100_blah = round(n_100_blah/5) ### number of obs randomly selected for testing data
n_200_blah = dim(blah_200)[1] ### total number of observations
n1_200_blah = round(n_200_blah/5) ### number of obs randomly selected for testing data

B = 100
TEALL_100 = NULL
TEALL_200 = NULL

for(b in 1:B) {
  # lin regression/KNN regression data splits
  flag_100 = sort(sample(1:n_100, n1_100))
  train_100 = clean_100[-flag_100,] ### training set
  test_100 = clean_100[flag_100,] ### testing set
  flag_200 = sort(sample(1:n_200, n1_200))
  train_200 = clean_200[-flag_200,] ### training set
  test_200 = clean_200[flag_200,] ### testing set
  
  # stepwise/lasso/ridge data splits
  flag_100_more = sort(sample(1:n_100_more, n1_100_more))
  train_100_more = clean_100_more[-flag_100_more,] ### training set
  test_100_more = clean_100_more[flag_100_more,] ### testing set
  flag_200_more = sort(sample(1:n_200_more, n1_200_more))
  train_200_more = clean_200_more[-flag_200_more,] ### training set
  test_200_more = clean_200_more[flag_200_more,] ### testing set
  
  ############### 1. linear regression ##########################
  ytrue_100 = test_100$avgprice
  linearmodel_100 <- lm(avgprice~., data = train_100)
  #linearmodeldf <- tidy(linearmodel)
  pred1a_100 <- predict(linearmodel_100, test_100[,-29])
  lm_100 <-   sqrt(mean((pred1a_100 - ytrue_100)^2))
  
  ytrue_200 = test_200$avgprice
  linearmodel_200 <- lm(avgprice~., data = train_200)
  #linearmodeldf <- tidy(linearmodel)
  pred1a_200 <- predict(linearmodel_200, test_200[,-29]);
  lm_200 <-   sqrt(mean((pred1a_200 - ytrue_200)^2)); 
  
  ############## 2. stepwise ##################################
  stepmodel_100 <- stepAIC(linearmodel_100, direction="both")
  pred2a_100 <- predict(stepmodel_100, test_100[,-29])
  step_100 <-   sqrt(mean((pred2a_100 - ytrue_100)^2)) 
  
  stepmodel_200 <- stepAIC(linearmodel_200, direction="both")
  pred2a_200 <- predict(stepmodel_200, test_200[,-29])
  step_200 <-   sqrt(mean((pred2a_200 - ytrue_200)^2))
  
  ############## 3. lasso #######################################
  lassomodel_100 <- lars(as.matrix(train_100_more[,1:51]), train_100_more[,52], 
                         type= "lasso", trace= TRUE)
  fit3_100 <- predict(lassomodel_100, as.matrix(test_100_more[,1:51]), s=1.3,
                      type="fit", mode="lambda");
  yhat3_100 <- fit3_100$fit; 
  lasso_100 <- sqrt(mean((yhat3_100 - test_100_more$avgprice)^2))
  
  lassomodel_200 <- lars(as.matrix(train_200_more[,1:51]), train_200_more[,52], 
                         type= "lasso", trace= TRUE)
  fit3_200 <- predict(lassomodel_200, as.matrix(test_200_more[,1:51]), s=1.3,
                      type="fit", mode="lambda");
  yhat3_200 <- fit3_200$fit; 
  lasso_200 <- sqrt(mean((yhat3_200 - test_200_more$avgprice)^2))
  
  ############## 4. ridge #########################################
#   blah_100_flag = sort(sample(1:n_100_blah, n1_100_blah))
#   blah_100_train <- blah_100[-blah_100_flag,] ### training set
#   blah_100_test = blah_100[blah_100_flag,] ### testing set
#   ridgemodel_100 <- lm.ridge(blah_100_train$avgprice~., data = blah_100_train, 
#                          lambda = seq(0, 100, 0.01))
#   lambdaopt_100 <- which.min(ridgemodel_100$GCV);
#   rig1coef_100 <- ridgemodel_100$coef[,lambdaopt_100];
#   # find the intercepts using ybar and xbar from training data
#   rig1intercepts_100 <- ridgemodel_100$ym - sum(ridgemodel_100$xm * (rig1coef_100 / ridgemodel_100$scales)); 
#   pred4_100 <- scale(blah_100_test[,1:49], center = F, scale = ridgemodel_100$scales)%*%rig1coef_100 + rig1intercepts_100
#   ridge_100 <- sqrt(mean((pred4_100 - ytrue_100)^2))
#   print(ridge_100)
  
#   # 200
#   blah_200_flag = sort(sample(1:n_200_blah, n1_200_blah))
#   blah_200_train <- blah_200[-blah_200_flag,] ### training set
#   blah_200_test = blah_200[blah_200_flag,] ### testing set
#   ridgemodel_200 <- lm.ridge(blah_200_train$avgprice~., data = blah_200_train, 
#                              lambda = seq(0, 100, 0.01))
#   lambdaopt_200 <- which.min(ridgemodel_200$GCV);
#   rig1coef_200 <- ridgemodel_200$coef[,lambdaopt_200];
#   # find the intercepts using ybar and xbar from training data
#   rig1intercepts_200 <- ridgemodel_200$ym - sum(ridgemodel_200$xm * (rig1coef_200 / ridgemodel_200$scales)); 
#   pred4_200 <- scale(blah_200_test[,1:49], center = F, scale = ridgemodel_200$scales)%*%rig1coef_200 + rig1intercepts_200
#   ridge_200 <- sqrt(mean((pred4_200 - ytrue_200)^2))
  
  ############### 5. KNN Regression ##################################
  knn_1 = knn.reg(train_100[,1:28], y=train_100$avgprice,k=1)
  knn_3 = knn.reg(train_100[,1:28], y=train_100$avgprice,k=3)
  knn_5 = knn.reg(train_100[,1:28], y=train_100$avgprice,k=5)
  knn_7 = knn.reg(train_100[,1:28], y=train_100$avgprice,k=7)
  knn_15 = knn.reg(train_100[,1:28], y=train_100$avgprice,k=15)
  knn1_100 <- sqrt(mean((knn_1$pred - ytrue_100)^2))
  knn3_100 <- sqrt(mean((knn_3$pred - ytrue_100)^2))
  knn5_100 <- sqrt(mean((knn_5$pred - ytrue_100)^2))
  knn7_100 <- sqrt(mean((knn_7$pred - ytrue_100)^2))
  knn15_100 <- sqrt(mean((knn_15$pred - ytrue_100)^2))
  
  knn_1 = knn.reg(train_200[,1:28], y=train_200$avgprice,k=1)
  knn_3 = knn.reg(train_200[,1:28], y=train_200$avgprice,k=3)
  knn_5 = knn.reg(train_200[,1:28], y=train_200$avgprice,k=5)
  knn_7 = knn.reg(train_200[,1:28], y=train_200$avgprice,k=7)
  knn_15 = knn.reg(train_200[,1:28], y=train_200$avgprice,k=15)
  knn1_200 <- sqrt(mean((knn_1$pred - ytrue_200)^2))
  knn3_200 <- sqrt(mean((knn_3$pred - ytrue_200)^2))
  knn5_200 <- sqrt(mean((knn_5$pred - ytrue_200)^2))
  knn7_200 <- sqrt(mean((knn_7$pred - ytrue_200)^2))
  knn15_200 <- sqrt(mean((knn_15$pred - ytrue_200)^2))
  
  ############# 6. Regression Tree #####################
  tree_100 = rpart(avgprice ~., data=train_100, control=rpart.control(cp=1e-04))
  pred_rt_100 = predict(tree_100, test_100[,1:28])
  rt_100 <-   sqrt(mean((pred_rt_100 - ytrue_100)^2))
  
  tree_200 = rpart(avgprice ~., data=train_200, control=rpart.control(cp=1e-04))
  pred_rt_200 = predict(tree_200, test_200[,1:28])
  rt_200 <-   sqrt(mean((pred_rt_200 - ytrue_200)^2))
  
  TEALL_100 = rbind(TEALL_100, cbind(lm_100, step_100, lasso_100, 
                                     knn1_100, knn3_100, knn5_100, knn7_100, 
                                     knn15_100, rt_100))
  TEALL_200 = rbind(TEALL_200, cbind(lm_200, step_200, lasso_200, 
                                     knn1_200, knn3_200, knn5_200, knn7_200, 
                                     knn15_200, rt_200))
}

means_100 <- apply(TEALL_100, 2, mean)
var_100 <- apply(TEALL_100, 2, var)
means_200 <- apply(TEALL_200, 2, mean)
var_200 <- apply(TEALL_200, 2, var)

write.csv(TEALL_100, "errors_100.csv")
write.csv(TEALL_200, "errors_200.csv")

# random forest test error values
rf_all <- c(10.199981, 9.513880, 11.238640, 12.369489, 10.273996, 11.371680, 10.157451, 10.286907, 11.925999,
10.847275, 11.327059, 11.504653, 12.288701,  9.830169, 12.566704, 10.902168, 12.441885, 12.739336,
10.261816, 10.406223,  9.328172, 10.030011, 11.818224, 11.769025, 11.545916, 12.002052, 12.748935,
10.757624,  9.897904, 10.826552, 10.972271, 11.537925, 11.919462, 13.263013, 11.162739, 11.885584,
11.542185,  9.736788, 10.778046, 11.333811, 11.211111, 11.176666, 11.836852, 10.708638, 11.114151,
11.837271, 11.353094, 11.457258, 11.500505, 14.061193, 10.358633, 10.517583, 12.950489, 12.359038,
11.945972, 11.822813, 10.920706,  8.724078, 10.473460, 12.422105, 11.274145, 11.012464, 11.011550,
11.515238, 11.372280, 11.359790,  9.922540, 10.712736, 11.681669, 10.867893, 12.409248, 12.295619,
10.761020, 10.543467, 10.916222, 11.640304, 10.249922, 10.633260, 10.178963,  9.886135, 12.639046,
11.501931, 11.285884, 12.844851, 12.281784, 12.177947, 12.950711, 11.744330, 10.738616, 11.545606,
11.052071, 10.191215, 12.818452, 12.350261, 11.569720, 11.423028, 10.467902,  9.448761, 10.588459,
13.311940)

errors = cbind(errors_100, errors_200, rf_all)

cols = dim(errors)[2]-1
t_tests = matrix(ncol=cols,nrow=1)

for (i in 1:cols) {
  result <- t.test(errors[,19], errors[,i], paired=TRUE)
  t_tests[1:i] <- result$p.value
}
# random forest is significantly better than everything else
t_tests = data.frame(t_tests)
colnames(t_tests) <- names(errors)[1:cols]
