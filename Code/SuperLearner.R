#Loading relevant packages
library(SuperLearner)
library(randomForest)
library(ggplot2)
library(tidyr)

########################################################
# R code for Adaptive Lasso for SuperLearner package   #
#                                                      #
# This code is an extension of the SuperLearner        #
# package. It enables the use of the adaptive lasso    #
# in the SuperLearner package                          #
#                                                      #
# Dependices: source Adaptive Lasso.R and              #
#                    HelperFunctions.R                 #
#                                                      #
########################################################



##############################################################
#           Adaptive lasso for SuperLearner package          #
##############################################################

#' Title
#'
#' @param Y Vector. Target variable.
#' @param X Data frame. Training data set
#' @param newX Data frame. Test data set
#' @param family Family type function.
#' @param regression_method Character. Method for obtaining weights: 'ols', 'ridge' or 'adaptive_weights'
#' @param predict_type Character. Prediction type
#' @param CV Boolean. Cross-validate to tune hyperparameters in adaptive.lasso fit.
#' @param gamma Numerical. Specify gamma value
#' @param lambda Numerical. Specify lambda value
#' @param nfolds Integer. Number of cross-validations
#' @param gamma_seq Vector. Gamma values for cross-validation. 
#' @param seed Set seed
#' @param ... other parameters
#'
#' @return Adaptive lasso fit for SuperLearner() and its predictions
#' @export 
#'
#' @examples
SL.adaptive.lasso <- function(Y, X, newX, family = gaussian(), regression_method = 'ols',
                              predict_type = 'response',
                              CV = T, gamma = 1, lambda = 1,
                              nfolds = 10,
                              gamma_seq = seq(0.01,2,0.5), 
                              seed = 1,...){
  
  
  if (is.data.frame(X)) {
    X = data.matrix(X)
  }
  
  fit.adaptive.lasso <- adaptive_lasso(X = X, Y = Y, 
                                       regression_method = regression_method,
                                       family = family,
                                       CV = CV,
                                       gamma = gamma,
                                       lambda = lambda,
                                       nfolds = nfolds,
                                       gamma_seq = gamma_seq,
                                       seed = seed)$adaptive_lasso
  
  if (is.data.frame(newX)) {
    newX = data.matrix(newX)
  }
  
  pred <- predict(fit.adaptive.lasso, newx = newX, type = predict_type)
  
  fit <- list(object = fit.adaptive.lasso, family = family)
  class(fit) <- "SL.adaptive.lasso"
  out <- list(pred = pred, fit = fit)
  return(out)
}




