library(glmnet)
library(mlbench)
library(caret)

#Both discrete and continuous outcome?



#Then use these beta estimates to construct the adaptive weights


#Loading data set to run models. The target variable is medv (median value of ower-occupied homes in USD 1000's)
data(BostonHousing)


ridge_regression <- function(data, target_column, family = gaussian(), lambda_seq = 10^seq(3, -3, by = -.1), seed = 1){
  set.seed(seed)
  
  if (class(target_column) != "character"){
    stop("The target column should be specified as a character")
  }
  
  # Extract covariates and target
  X <- data.matrix(BostonHousing[, -which(names(BostonHousing) == target_column)])
  y <- data.matrix(BostonHousing[, which(names(BostonHousing) == target_column)])
  
  # Fit the ridge regression using cross validation to obtain best lambda value
  ridge_cv <- cv.glmnet(X, y, alpha = 0, lambda = lambda_seq, family = family, standardize=TRUE)
  
  # Best lambda value
  best_lambda <- ridge_cv$lambda.min
  
  #Fitting final model
  best_ridge <- glmnet(X, y, alpha = 0, lambda = best_lambda, standardize=TRUE)
  
  return(best_ridge)
}

ridge_regression(BostonHousing, 'medv')



ols_regression <- function(data, target_column){
  
  X <- colnames(BostonHousing[colnames(BostonHousing) != target_column])
  
  # Construct formula for OLS regression
  formula <- as.formula(paste(target_column, "~", paste(X, collapse = "+")))
  
  # Fit OLS regression model
  ols_model <- lm(formula, data = data)

  return(ols_model)
}


adaptive_lasso <- function(data, target_column, regression_method, family = gaussian(), gamma_seq = c(1), seed = 1,...){
  set.seed(seed)
  
  if (regression_method == 'OLS'){
    initial_regression <- ols_regression(data, target_column)
  } else if (regression_method == 'ridge'){
    initial_regression <- ridge_regression(data, target_column, family = family)
  } else {
    stop("Invalid regression method. Choose between OLS or ridge")
  }
  
  # Extract coefficients from initial regression
  initial_regression_coefs <- coef(initial_regression)
  
  # Calculate adaptive weights
  adaptive_weights <- 1/(abs(initial_regression_coefs)^gamma)
  
  # Fitting lasso with penalty
  cv.lasso <- cv.glmnet(x, y, family=family, alpha=1, parallel=TRUE, standardize=TRUE, penalty.factor=adaptive_weights)
  
}

#We need to cross validate for values of gamma
#We need to implement code so that we can use the adaptive lasso for both regression and classification tasks
#Consider if we also need to specify penalty for the model
#Consider including GLMs in the adaptive Lasso as well (maybe instead of OLS simply fit a GLM where you can specify gaussian as family)







