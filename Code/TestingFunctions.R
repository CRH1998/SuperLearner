

#######################################
#         BOSTON HOUSING              #
#######################################

data(BostonHousing)


#use 70% of dataset as training set and 30% as test set
sample_boston <- sample(c(TRUE, FALSE), nrow(BostonHousing), replace=TRUE, prob=c(0.7,0.3))
Boston_train  <- BostonHousing[sample_boston, ]
Boston_test   <- BostonHousing[!sample_boston, ]

X_boston_train <- data.matrix(Boston_train[,colnames(Boston_train) != 'medv'])
Y_boston_train <- data.matrix(Boston_train[,colnames(Boston_train) == 'medv'])

X_boston_test <- data.matrix(Boston_test[,colnames(Boston_test) != 'medv'])
Y_boston_test <- data.matrix(Boston_test[,colnames(Boston_test) == 'medv'])


rr_boston <- ridge_regression(X_boston_train, Y_boston_train)
ols_boston <- ols_regression(X_boston_train, Y_boston_train)
adap_lasso_boston <- adaptive_lasso(X_boston_train, Y_boston_train, regression_method = 'adaptive_weights', 
                                    lambda_seq = 10^seq(5, -5, by = -.1), gamma_seq = seq(0.01,5,0.25))
SL_boston <- SuperLearner(Y = Y_boston_train, X = data.frame(X_boston_train), newX = X_boston_test, family = gaussian(),
             SL.library = c('SL.ridge', 'SL.adaptive.lasso', 'SL.xgboost', 'SL.lm'))



rr_boston_pred <- predict(rr_boston, newx = X_boston_test)
ols_boston_pred <- predict(ols_boston, newdata = data.frame(X_boston_test))
adap_lasso_boston_pred <- predict(adap_lasso_boston$adaptive_lasso, newx = X_boston_test)


mean((rr_boston_pred - Y_boston_test)^2)
mean((ols_boston_pred - Y_boston_test)^2)
mean((adap_lasso_boston_pred - Y_boston_test)^2)
mean((SL_boston$SL.predict - Y_boston_test)^2)




View(cbind(rr_boston_pred, ols_boston_pred, adap_lasso_boston_pred, SL_boston$SL.predict, Y_boston_test))







#######################################
#         BreastCancer data           #
#######################################

data(BreastCancer)
BreastCancer <- na.omit(BreastCancer)

#use 70% of dataset as training set and 30% as test set
sample_BreastCancer <- sample(c(TRUE, FALSE), nrow(BreastCancer), replace=TRUE, prob=c(0.7,0.3))
BreastCancer_train  <- BreastCancer[sample_BreastCancer, ]
BreastCancer_test   <- BreastCancer[!sample_BreastCancer, ]

X_BreastCancer_train <- data.matrix(BreastCancer_train[,colnames(BreastCancer_train) != 'Class'])
Y_BreastCancer_train <- ifelse(data.matrix(BreastCancer_train[,colnames(BreastCancer_train) == 'Class']) == "malignant", 1, 0)

X_BreastCancer_test <- data.matrix(BreastCancer_test[,colnames(BreastCancer_test) != 'Class'])
Y_BreastCancer_test <- ifelse(data.matrix(BreastCancer_test[,colnames(BreastCancer_test) == 'Class']) == "malignant", 1, 0)



rr_BreastCancer         <- ridge_regression(X_BreastCancer_train, Y_BreastCancer_train, family = 'binomial')
ols_BreastCancer        <- ols_regression(X_BreastCancer_train, Y_BreastCancer_train, family = 'binomial')
adap_lasso_BreastCancer <- adaptive_lasso(X_BreastCancer_train, Y_BreastCancer_train, family = 'binomial', regression_method = 'ridge', 
                                  nfolds = 10, lambda_seq = 10^seq(5, -5, by = -.1), gamma_seq = seq(0.01,5,0.05))
SL_BreastCancer         <- SuperLearner(Y = Y_BreastCancer_train, X = data.frame(X_BreastCancer_train), newX = X_BreastCancer_test, family = 'binomial', 
                                SL.library = c('SL.adaptive.lasso', 'SL.xgboost', 'SL.randomForest', 'SL.ksvm'))


rr_BreastCancer_pred <- predict(rr_BreastCancer, newx = X_BreastCancer_test, type = 'response') > 0.5
ols_BreastCancer_pred <- predict(ols_BreastCancer, newdata = data.frame(X_BreastCancer_test), type = 'response') > 0.5
adap_lasso_BreastCancer_pred <- predict(adap_lasso_BreastCancer$adaptive_lasso, newx = X_BreastCancer_test, type = 'response') > 0.5
SL_BreastCancer_pred <- SL_BreastCancer$SL.predict > 0.5

data.frame('ridge' = rr_BreastCancer_pred, 
           'OLS' = ols_BreastCancer_pred,
           'adaptive_lasso' = adap_lasso_BreastCancer_pred,
           'SuperLearner' = SL_BreastCancer_pred, 
           'Label' = Y_BreastCancer_test)


sum(rr_BreastCancer_pred == Y_BreastCancer_test)/length(rr_BreastCancer_pred)
sum(ols_BreastCancer_pred == Y_BreastCancer_test)/length(ols_BreastCancer_pred)
sum(adap_lasso_BreastCancer_pred == Y_BreastCancer_test)/length(adap_lasso_BreastCancer_pred)
sum(SL_BreastCancer_pred == Y_BreastCancer_test)/length(SL_BreastCancer_pred)










#######################################
#         Heart surgery data          #
#######################################

heart_surgery <- read_csv("C:/Users/brf337/Desktop/Super Learner/Datasets/Heart disease/heart_statlog_cleveland_hungary_final.csv", 
                                                  col_types = cols(sex = col_factor(levels = c("0", "1")), 
                                                                   `chest pain type` = col_factor(levels = c("1", "2", "3", "4")), 
                                                                   `fasting blood sugar` = col_factor(levels = c("0", "1")), 
                                                                   `resting ecg` = col_factor(levels = c("0", "1", "2")), 
                                                                   `exercise angina` = col_factor(levels = c("0", "1")), 
                                                                   `ST slope` = col_factor(levels = c("0", "1", "2", "3")), 
                                                                   target = col_factor(levels = c("0", "1"))))




#use 70% of dataset as training set and 30% as test set
sample_heart_surgery <- sample(c(TRUE, FALSE), nrow(heart_surgery), replace=TRUE, prob=c(0.7,0.3))
heart_surgery_train  <- heart_surgery[sample_heart_surgery, ]
heart_surgery_test   <- heart_surgery[!sample_heart_surgery, ]

X_heart_surgery_train <- data.matrix(heart_surgery_train[,colnames(heart_surgery_train) != 'target'])
Y_heart_surgery_train <- as.numeric(heart_surgery_train[,colnames(heart_surgery_train) == 'target']$target) - 1

X_heart_surgery_test <- data.matrix(heart_surgery_test[,colnames(heart_surgery_test) != 'target'])
Y_heart_surgery_test <- as.numeric(heart_surgery_test[,colnames(heart_surgery_test) == 'target']$target) - 1



rr_heart_surgery         <- ridge_regression(X_heart_surgery_train, Y_heart_surgery_train, family = 'binomial')
ols_heart_surgery        <- ols_regression(X_heart_surgery_train, Y_heart_surgery_train, family = 'binomial')
adap_lasso_heart_surgery <- adaptive_lasso(X_heart_surgery_train, Y_heart_surgery_train, family = 'binomial', regression_method = 'ols', 
                                          nfolds = 10, lambda_seq = 10^seq(5, -5, by = -.1), gamma_seq = seq(0.01,5,0.05))
SL_heart_surgery         <- SuperLearner(Y = Y_heart_surgery_train, X = data.frame(X_heart_surgery_train), newX = data.frame(X_heart_surgery_test), family = 'binomial', 
                                        SL.library = c('SL.adaptive.lasso', 'SL.xgboost', 'SL.randomForest', 'SL.ksvm'))


rr_heart_surgery_pred <- predict(rr_heart_surgery, newx = X_heart_surgery_test, type = 'response') > 0.5
ols_heart_surgery_pred <- predict(ols_heart_surgery, newdata = as.data.frame(X_heart_surgery_test), type = 'response') > 0.5
adap_lasso_heart_surgery_pred <- predict(adap_lasso_heart_surgery$adaptive_lasso, newx = X_heart_surgery_test, type = 'response') > 0.5
SL_heart_surgery_pred <- SL_heart_surgery$SL.predict > 0.5

data.frame('ridge' = rr_heart_surgery_pred, 
           'OLS' = ols_heart_surgery_pred,
           'adaptive_lasso' = adap_lasso_heart_surgery_pred,
           'SuperLearner' = SL_heart_surgery_pred, 
           'Label' = Y_heart_surgery_test)


sum(rr_heart_surgery_pred == Y_heart_surgery_test)/length(rr_heart_surgery_pred)
sum(ols_heart_surgery_pred == Y_heart_surgery_test)/length(ols_heart_surgery_pred)
sum(adap_lasso_heart_surgery_pred == Y_heart_surgery_test)/length(adap_lasso_heart_surgery_pred)
sum(SL_heart_surgery_pred == Y_heart_surgery_test)/length(SL_heart_surgery_pred)













