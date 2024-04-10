library(glmnet)
library(mlbench)
library(caret)
library(mltools)
library(data.table)
library(tidyverse)


###########################################
#   HELPER FUNCTIONS FOR SUPERLEARNER     #
#                                         #
#                                         #
#                                         #
#                                         #
#                                         #
#                                         #
#                                         #
#                                         #
###########################################


#Function for getting columntypes from a dataframe
get_column_types <- function(data){
  numerical_columns <- character(0)
  categorical_columns <- character(0)
  
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      numerical_columns <- c(numerical_columns, col)
    } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
      categorical_columns <- c(categorical_columns, col)
    }
  }
  
  return(list(numerical = numerical_columns, categorical = categorical_columns))
}


#Function for splitting data set into categorical and numerical variables
divide_data_into_num_cat <- function(data){
  
  #Get column types
  data_column_types <- get_column_types(data)
  
  numerical_columns_names <- data_column_types$numerical
  categorical_columns_names <- data_column_types$categorical
  
  #Split data set into categorical and numerical variables
  numerical_columns   <- data[numerical_columns_names]
  categorical_columns <- data[categorical_columns_names]
  
  return(list(numerical_columns = numerical_columns, categorical_columns = categorical_columns))
}


#Function for one-hot encoding categorical variables of data set
one_hot_encode_categorical <- function(data, remove_intercept = T){
  divided_data_set <- divide_data_into_num_cat(data)
  
  categorical_columns <- divided_data_set$categorical_columns
  numerical_columns   <- divided_data_set$numerical_columns
  
  if(remove_intercept == T){
    one_hot_encoded <- one_hot(as.data.table(categorical_columns))
  } else {
    one_hot_encoded <- model.matrix(~categorical_columns)
  }
  
  encoded_dataframe <- as.data.frame(cbind(one_hot_encoded, numerical_columns))
  
  return(encoded_dataframe)
}
