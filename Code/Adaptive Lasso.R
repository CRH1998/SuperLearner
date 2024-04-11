library(glmnet)
library(mlbench)
library(caret)
library(xgboost)
library(mltools)
library(data.table)
library(tidyverse)
library(xgboost)


###########################################
#       R code for Adaptive Lasso         #
#                                         #
#                                         #
#                                         #
#                                         #
#                                         #
#                                         #
#                                         #
#                                         #
###########################################




#-------------------Ridge regression--------------------------

ridge_regression <- function(X, Y, 
                             family = gaussian(), 
                             lambda_seq = 10^seq(3, -3, by = -.1), 
                             seed = 1){
  set.seed(seed)
  
  
  if (is.data.frame(X)) {
    X = data.matrix(X)
  }
  
  if (typeof(Y) == 'list'){
    Y <- Y[[1]]
  }
  

  # Apply cross validation to obtain best lambda value
  ridge_cv <- cv.glmnet(X, Y, 
                        alpha = 0, 
                        family = family, 
                        lambda = lambda_seq, 
                        standardize = TRUE, #standardize dataset
                        intercept = FALSE)  #remove intercept
  
  
  # Best lambda value
  best_lambda <- ridge_cv$lambda.min
  
  
  #Fitting final model
  best_ridge <- glmnet(X, Y, alpha = 0, family = family, lambda = best_lambda, standardize=TRUE, intercept = FALSE)
  
  return(best_ridge)
}





#--------------------OLS regression-----------------------------


ols_regression <- function(X, Y, family = gaussian()){
  
  if (is.matrix(X)) {
    X = as.data.frame(X)
  }
  
  if (typeof(Y) == 'list'){
    Y <- Y[[1]]
  }
  
  # Fit OLS regression model
  ols_model <- glm(Y ~ . - 1, data = X, family = family) #-1 removes intercept

  return(ols_model)
}








##############################################################
#                     Adaptive lasso                         #
##############################################################



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

adaptive_lasso <- function(X, Y, regression_method, family = gaussian(), 
                           nfolds = 10, lambda_seq = 1.5^seq(3, -3, by = -.1), 
                           gamma_seq = seq(0.1,5,0.25), seed = 1,...){
  
  set.seed(seed)
  
  
  #-----------Running initial regression-------------
  
  if (regression_method == 'ols'){
    writeLines("Running ols regression for initial beta estimates")
    initial_regression <- ols_regression(X, Y, family = family)
    
    # Extract coefficients from initial regression
    initial_regression_coefs <- coef(initial_regression)
    
    writeLines("Done")
  } else if (regression_method == 'ridge'){
    writeLines("Running ridge regression for initial beta estimates")
    initial_regression <- ridge_regression(X, Y, family = family)
    
    # Extract coefficients from initial regression
    initial_regression_coefs <- initial_regression$beta
    
    writeLines("Done")
  } else if (regression_method == 'adaptive_weights'){
    
  } else {
    stop("Invalid regression method. Choose between ols, ridge or adaptive_weights")
  }
  
  writeLines(paste0("Number of covariates: ", ncol(X)))
  writeLines(paste0("Number of estimated regression coefficients: ",length(initial_regression_coefs)))
  
  
  #----------------------------------------------------
  
  
  
  #---------------Fitting adaptive lasso---------------
  
  # Divide data into target and covariates (think about what to do with intercept/categorical variables)
  #X <- data.matrix(cbind(1, X)) #cbind(1,...) to account for intercept
  

  
  writeLines(paste0("Running ", nfolds,"-fold cross validation to tune gamma and lambda parameter in adaptive lasso for ", 
                    length(gamma_seq), ' x ', length(lambda_seq),' = ',length(gamma_seq) * length(lambda_seq), ' different parameter combinations'))
  
  
  
  # Running cross-validation to tune gamma and lambda
  cv_result_df <- data.frame(col1 = numeric(0),col2 = numeric(0),col3 = numeric(0))

  x <- 0  

  for (gamma in gamma_seq){
    
    #Calculate adaptive weights for current gamma value
    adaptive_weights <- 1/(abs(initial_regression_coefs)^gamma)
    
    #print(adaptive_weights)
    
    #Run cross validation with current adaptive weights and differenct lambda values
    cv.lasso <- cv.glmnet(x = X, y = Y, alpha = 1,
                          family = family,
                          lambda = lambda_seq,
                          nfolds = nfolds, parallel=TRUE, standardize=TRUE, 
                          penalty.factor = adaptive_weights)
    
    #Storing cross validation result
    cv_result_df <- rbind(cv_result_df, c(cv.lasso$lambda.min, min(cv.lasso$cvm), gamma))
    
    x <- x + 1
    
    progress(x = x, max = length(gamma_seq))
    
  }
  
  #Renaming cv_result_df
  colnames(cv_result_df) <- c("lambda.min", "cvm", "gamma")
  
  
  #Retrieving best values from cross validation
  best_index <- which.min(cv_result_df$cvm)
  best_lambda <- cv_result_df$lambda.min[best_index]
  best_gamma <- cv_result_df$gamma[best_index]
  best_adaptive_weights <- 1/(abs(initial_regression_coefs)^best_gamma)
  
  writeLines("Cross validation done")
  writeLines(c(paste("Best parameters found"), paste0('Gamma: ', best_gamma), paste0('Lambda: ', best_lambda)))
  
  
  #Fitting final adaptive lasso using best lambda and best adaptive weights from cross validation
  best_adaptive_lasso <- glmnet(x = X, y = Y, family = family, alpha = 1, 
                                lambda = best_lambda, 
                                penalty.factor = best_adaptive_weights,
                                standardize = TRUE)
  
  return(list("adaptive_lasso" = best_adaptive_lasso, 
              "initial_regression" = initial_regression, 
              "best_lambda" = best_lambda,
              "best_gamma" = best_gamma,
              "best_adaptive_weights" = best_adaptive_weights,
              "cv.result" = cv_result_df))
}






















