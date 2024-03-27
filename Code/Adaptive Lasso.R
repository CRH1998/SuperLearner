library(glmnet)
library(mlbench)
library(caret)

#Both discrete and continuous outcome?



#Then use these beta estimates to construct the adaptive weights


#Loading data set to run models. The target variable is medv (median value of ower-occupied homes in USD 1000's)
data(BostonHousing)


ridge_regression <- function(data, target_column, lambda_seq = 10^seq(3, -3, by = -.1), seed = 1){
  set.seed(seed)
  
  if (class(target_column) != "character"){
    stop("The target column should be specified as a character")
  }
  
  # Extract covariates and target
  X <- data.matrix(BostonHousing[, -which(names(BostonHousing) == target_column)])
  y <- data.matrix(BostonHousing[, which(names(BostonHousing) == target_column)])
  
  # Fit the ridge regression using cross validation to obtain best lambda value
  ridge_cv <- cv.glmnet(X, y, alpha = 0, lambda = lambda_seq, standardize=TRUE)
  
  # Best lambda value
  best_lambda <- ridge_cv$lambda.min
  
  #Fitting final model
  best_ridge <- glmnet(X, y, alpha = 0, lambda = best_lambda, standardize=TRUE)
  
  return(best_ridge)
}


ols_regression <- function(data, target_column){
  
  X <- colnames(BostonHousing[colnames(BostonHousing) != target_column])
  
  # Construct formula for OLS regression
  formula <- as.formula(paste(target_column, "~", paste(X, collapse = "+")))
  
  # Fit OLS regression model
  ols_model <- lm(formula, data = data)

  return(ols_model)
}


