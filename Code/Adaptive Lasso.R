library(glmnet)
library(mlbench)
library(caret)
library(xgboost)
library(mltools)
library(data.table)
library(tidyverse)
library(xgboost)
library(MESS)
library(MASS)
library(kernlab)
library(readr)


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
                        standardize = TRUE) #standardize dataset
  
  
  # Best lambda value
  best_lambda <- ridge_cv$lambda.min
  
  
  #Fitting final model
  best_ridge <- glmnet(X, Y, 
                       alpha = 0, 
                       family = family, 
                       lambda = best_lambda, 
                       standardize=TRUE)
  
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
  ols_model <- glm(Y ~ ., data = X, family = family)

  return(ols_model)
}








##############################################################
#                     Adaptive lasso                         #
##############################################################



#' @title adaptive lasso
#' @description fits the adaptive lasso to a data set
#' @param X data set (dataframe)
#' @param Y target variable (character)
#' @param regression_method ols or ridge to initial regression for adaptive weights (character)
#' @param family family object for regression (family object)
#' @param CV cross-validate to find best lambda and gamma parameter (boolean)
#' @param nfolds number of cross validation folds (double)
#' @param gamma_seq sequence of gamma values for cross validation (double)
#' @param gamma gamma value if CV = F (double)
#' @param lambda lambda value if CV = F (double)
#' @return adaptive lasso and key characteristics (list)

adaptive_lasso <- function(X, Y, regression_method, family = gaussian(), CV = T,
                           nfolds = 10, gamma_seq = seq(0.1,5,0.25), gamma = 1, lambda = 1, seed = 1,...){
  
  set.seed(seed)
  
  
  #-----------Running initial regression-------------
  
  
  if(ncol(X) > nrow(X)){
    writeLines("Warning: Number of coefficients is greater than number of rows. Using adaptive.weights() to find adaptive weigths.")
    initial_regression <- 'No initial regression - adaptive.weights() was used.'
  } else {
    
    if (regression_method == 'ols'){
      writeLines("Running ols regression for initial beta estimates")
      initial_regression <- ols_regression(X, Y, family = family)
      
      # Extract coefficients from initial regression
      initial_regression_coefs <- coef(initial_regression)[-1]
      
      
      writeLines("Done")
    } else if (regression_method == 'ridge'){
      writeLines("Running ridge regression for initial beta estimates")
      initial_regression <- ridge_regression(X, Y, family = family)
      
      # Extract coefficients from initial regression
      initial_regression_coefs <- coef(initial_regression)[-1]
      
      
      writeLines("Done")
    } else if (regression_method == 'adaptive_weights'){
      initial_regression <- 'No initial regression - adaptive.weights() was used.'
    } else {
      stop("Invalid regression method. Choose between ols, ridge or adaptive_weights")
    }
  }
  
  #----------------------------------------------------
  
  
  
  
  
  #---------------Fitting adaptive lasso---------------
  
  # Run cross-validation if chosen
  if (CV){
    
    writeLines(paste0("Running ", nfolds,"-fold cross validation to tune gamma and lambda parameter in adaptive lasso"))
    
    # Running cross-validation to tune gamma and lambda
    cv_result_df <- data.frame(col1 = numeric(0),col2 = numeric(0),col3 = numeric(0))
    
    
    #For tracking cross validation progress
    x <- 0
    
    
    for (gamma in gamma_seq){
      
      #Calculate adaptive weights for current gamma value
      
      if (regression_method == 'adaptive_weights' | ncol(X) > nrow(X)){
        adaptive_weights <- adaptive.weights(X, Y, nu = gamma, weight.method = "univariate")$weights
      } else {
        adaptive_weights <- 1/(abs(initial_regression_coefs)^gamma)
      }
      
      
      #Run cross validation with current adaptive weights and different lambda values
      cv.lasso <- cv.glmnet(x = X, y = Y, alpha = 1,
                            family = family,
                            nfolds = nfolds, parallel=TRUE, standardize=TRUE, 
                            penalty.factor = adaptive_weights)
      
      #Store cross validation result
      cv_result_df <- rbind(cv_result_df, c(cv.lasso$lambda.min, gamma, min(cv.lasso$cvm)))
      
      
      #For tracking cross-validation progress
      x <- x + 1
      
      progress(x = x, max = length(gamma_seq))
      
    }
    
    
    #Rename cv_result_df
    colnames(cv_result_df) <- c("lambda.min", "gamma", "cvm")
    
    
    #Retrieve best values from cross validation
    best_index <- which.min(cv_result_df$cvm)
    lambda <- cv_result_df$lambda.min[best_index]
    gamma <- cv_result_df$gamma[best_index]
  
    
  # Skip cross-validation if chosen  
  } else {
    cv_result_df <- 'No cross-validation'
  }
  
  
  
  # Calculate adaptive weights
  
  if (regression_method == 'adaptive_weights'){
    best_adaptive_weights <- adaptive.weights(X, Y, nu = gamma, weight.method = "univariate")$weights
  } else {
    best_adaptive_weights <- 1/(abs(initial_regression_coefs)^gamma)
  }
  
  
  
  #Fit final adaptive lasso using best lambda and best adaptive weights from cross validation or pre-specified values of lambda and gamma
  
  writeLines(paste0("Fitting adaptive lasso with lambda ", lambda," and gamma ", gamma))
  
  best_adaptive_lasso <- glmnet(x = X, y = Y, family = family, alpha = 1, 
                                lambda = lambda, 
                                penalty.factor = best_adaptive_weights,
                                standardize = TRUE)
  writeLines('Done')
  
  return(list("adaptive_lasso" = best_adaptive_lasso, 
              "initial_regression" = initial_regression, 
              "best_lambda" = lambda,
              "best_gamma" = gamma,
              "best_adaptive_weights" = best_adaptive_weights,
              "cv.result" = cv_result_df))
}






















