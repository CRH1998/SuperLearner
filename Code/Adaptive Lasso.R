library(glmnet)
library(mlbench)
library(caret)
library(gcdnet)


#Loading data set to run models. The target variable is medv (median value of ower-occupied homes in USD 1000's)

data("BostonHousing")



ridge_regression <- function(data, target_column, 
                             family = gaussian(), lambda_seq = 10^seq(3, -3, by = -.1), 
                             type.measure = "default", seed = 1){
  set.seed(seed)
  
  if (class(target_column) != "character"){
    stop("The target column should be specified as a character")
  }
  
  # Extract covariates and target
  X <- data.matrix(data[, -which(names(data) == target_column)])
  y <- data.matrix(data[, which(names(data) == target_column)])
  
  
  # Fit the ridge regression using cross validation to obtain best lambda value
  ridge_cv <- cv.glmnet(X, y, 
                        alpha = 0, 
                        lambda = lambda_seq, 
                        family = family, 
                        type.measure = type.measure,
                        standardize = TRUE, #standardize dataset
                        intercept = FALSE)  #remove intercept
  
  # Best lambda value
  best_lambda <- ridge_cv$lambda.min
  
  #Fitting final model
  best_ridge <- glmnet(X, y, alpha = 0, lambda = best_lambda, standardize=TRUE)
  
  return(best_ridge)
}




ols_regression <- function(data, target_column, family = gaussian()){
  
  X <- colnames(data[colnames(data) != target_column])
  
  # Construct formula for OLS regression
  formula <- as.formula(paste(target_column, "~", paste(X, collapse = "+")))
  
  # Fit OLS regression model
  ols_model <- glm(formula, data = data, family = family)

  return(ols_model)
}





#' @title adaptive lasso
#' @description fits the adaptive lasso to a data set
#' @param data data set (dataframe)
#' @param target_column target variable (character)
#' @param regression_method ols or ridge to initial regression for adaptive weights (character)
#' @param family family object for regression (family object)
#' @param type.measure evaluation metric for cross validation (character)
#' @param nfolds number of cross validation folds (double)
#' @param lambda_seq sequence of lambda values for cross validation (double)
#' @param gamma_seq sequence of gamma values for cross validation (double)
#' @return adaptive lasso and key characteristics

adaptive_lasso <- function(data, target_column, regression_method, family = gaussian(), 
                           type.measure = "default", nfolds = 10, lambda_seq = 10^seq(3, -3, by = -.1), 
                           gamma_seq = seq(0.1,3,0.25), seed = 1,...){
  
  set.seed(seed)
  
  
  #-----------Running initial regression-------------
  
  if (regression_method == 'ols'){
    initial_regression <- ols_regression(data, target_column, family = family)
  } else if (regression_method == 'ridge'){
    initial_regression <- ridge_regression(data, target_column, family = family)
  } else {
    stop("Invalid regression method. Choose between ols or ridge")
  }
  
  # Extract coefficients from initial regression
  initial_regression_coefs <- coef(initial_regression)
  
  #----------------------------------------------------
  
  
  
  
  #---------------Fitting adaptive lasso---------------
  
  # Divide data into target and covariates (think about what to do with intercept/categorical variables)
  X <- data.matrix(cbind(1, data[, -which(names(data) == target_column)])) #cbind(1,...) to account for intercept
  y <- data.matrix(data[, which(names(data) == target_column)])
  
  
  # Running cross-validation to tune gamma and lambda
  cv_result_df <- data.frame(col1 = numeric(0),col2 = numeric(0),col3 = numeric(0))
  
  for (gamma in gamma_seq){
    
    #Calculate adaptive weights for current gamma value
    adaptive_weights <- 1/(abs(initial_regression_coefs)^gamma)
    
    #Run cross validation with current adaptive weights and differenct lambda values
    cv.lasso <- cv.glmnet(X, y, family=gaussian(), alpha=1, lambda = lambda_seq, type.measure = type.measure,
                          nfolds = 10, parallel=TRUE, standardize=TRUE, penalty.factor = adaptive_weights)
    
    #Storing cross validation result
    cv_result_df <- rbind(cv_result_df, c(cv.lasso$lambda.min, min(cv.lasso$cvm), gamma))
  }
  
  #Renaming cv_result_df
  colnames(cv_result_df) <- c("lambda.min", "cvm", "gamma")
  
  
  #Retrieving best values from cross validation
  best_index <- which.min(cv_result_df$cvm)
  best_lambda <- cv_result_df$lambda.min[best_index]
  best_gamma <- cv_result_df$gamma[best_index]
  best_adaptive_weights <- 1/(abs(initial_regression_coefs)^best_gamma)
  
  #Fitting final adaptive lasso using best lambda and best adaptive weights from cross validation
  best_adaptive_lasso <- glmnet(X, y, familiy = familiy, alpha = 1, 
                                lambda = best_lambda, 
                                penalty.factor = best_adaptive_weights * mean(best_adaptive_weights),
                                standardize = TRUE)
  
  return(list("adaptive_lasso" = best_adaptive_lasso, 
              "initial_regression" = initial_regression, 
              "best_lambda" = best_lambda,
              "best_gamma" = best_gamma,
              "best_adaptive_weights" = best_adaptive_weights,
              "cv.result" = cv_result_df))
}

adap_lasso <- adaptive_lasso(BostonHousing, 'medv', regression_method = 'ols')
best_adaptive_weights <- adap_lasso$best_adaptive_weights
own_lasso_beta <- adap_lasso$adaptive_lasso$beta

initial_regression <- ridge_regression(BostonHousing, 'medv', family = gaussian())
initial_regression_coefs <- initial_regression$beta

X <- data.matrix(cbind(1, BostonHousing[, -which(names(BostonHousing) == 'medv')])) #cbind(1,...) to account for intercept
y <- data.matrix(BostonHousing[, which(names(BostonHousing) == 'medv')])


test_lasso <- gcdnet(X, y,lambda2 = 0, lambda = adap_lasso$best_lambda, method = 'ls', pf = best_adaptive_weights)
test_lasso2 <- glmnet(X, y, alpha = 1, lambda = adap_lasso$best_lambda, family = gaussian(), penalty.factor = best_adaptive_weights)


