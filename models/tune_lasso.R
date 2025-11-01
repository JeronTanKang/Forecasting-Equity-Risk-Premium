## this file gets the plug in lambda chosen by rlasso()  for h=1,3,6,12

#load library
library(fredr)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo) 
library(lubridate)
library(data.table)
library(hdm)
library(glmnet)
library(forecast)
library(readxl)
library(urca)
library(ggplot2)

#read in data
file_path = "../data/stationary_indicators.csv"
df = read.csv(file_path)
#function to run lasso and pick out important indicators for 1 step forecast
tune_lasso_h1 <- function(df) {
  df_std = df %>% mutate(across(-c(date, erp), ~ as.numeric(scale(.)))) %>% arrange(desc(date))
  df_std = df_std %>% mutate(target = lag(erp, n = 1)) %>% select(date, target, everything()) %>% drop_na()
  
  #target variable 
  y = df_std %>% select(target)
  
  #predictors
  x = df_std %>% select(-target, -date)
  
  #lasso
  lasso_model <- rlasso(x, y, penalty=list(X.dependent.lambda=TRUE), post= FALSE)
  lasso_coeffs <- coef(lasso_model)
  
  # Convert to a data frame
  coeff_df <- data.frame(variable = names(lasso_coeffs), coefficient = as.numeric(lasso_coeffs))
  
  # Remove intercept and filter non-zero coefficients
  selected_vars <- coeff_df %>%
    filter(coefficient != 0, variable != "(Intercept)") %>% mutate(Importance = abs(coefficient)) %>%
    arrange(desc(Importance)) %>% select(variable) %>% pull(variable) %>% as.character()
  
  penalty <- lasso_model$lambda0
  return(penalty)
}
tune_lasso_h1(df)

tune_lasso_h3 <- function(df) {
  df_std = df %>% mutate(across(-c(date, erp), ~ as.numeric(scale(.)))) %>% arrange(desc(date))
  df_std = df_std %>% mutate(target = lag(erp, n = 3)) %>% select(date, target, everything()) %>% drop_na() %>% arrange(date)
  
  #target variable 
  y = df_std %>% select(target)
  
  #predictors
  x = df_std %>% select(-target, -date)
  
  #lasso
  lasso_model <- rlasso(x, y, penalty=list(X.dependent.lambda=TRUE), post= FALSE)
  lasso_coeffs <- coef(lasso_model)
  
  # Convert to a data frame
  coeff_df <- data.frame(variable = names(lasso_coeffs), coefficient = as.numeric(lasso_coeffs))
  
  # Remove intercept and filter non-zero coefficients
  selected_vars <- coeff_df %>%
    filter(coefficient != 0, variable != "(Intercept)") %>% mutate(Importance = abs(coefficient)) %>%
    arrange(desc(Importance)) %>% select(variable) %>% pull(variable) %>% as.character()
  penalty <- lasso_model$lambda0
  return(penalty)
}
tune_lasso_h3(df)

tune_lasso_h6 <- function(df) {
  df_std = df %>% mutate(across(-c(date, erp), ~ as.numeric(scale(.)))) %>% arrange(desc(date))
  df_std = df_std %>% mutate(target = lag(erp, n = 6)) %>% select(date, target, everything()) %>% drop_na() %>% arrange(date)
  
  #target variable 
  y = df_std %>% select(target)
  
  #predictors
  x = df_std %>% select(-target, -date)
  
  #lasso
  lasso_model <- rlasso(x, y, penalty=list(X.dependent.lambda=TRUE), post= FALSE)
  lasso_coeffs <- coef(lasso_model)
  
  # Convert to a data frame
  coeff_df <- data.frame(variable = names(lasso_coeffs), coefficient = as.numeric(lasso_coeffs))
  
  # Remove intercept and filter non-zero coefficients
  selected_vars <- coeff_df %>%
    filter(coefficient != 0, variable != "(Intercept)") %>% mutate(Importance = abs(coefficient)) %>%
    arrange(desc(Importance)) %>% select(variable) %>% pull(variable) %>% as.character()
  
  penalty <- lasso_model$lambda0
  return(penalty)
}
tune_lasso_h6(df)

tune_lasso_h12 <- function(df) {
  df_std = df %>% mutate(across(-c(date, erp), ~ as.numeric(scale(.)))) %>% arrange(desc(date))
  df_std = df_std %>% mutate(target = lag(erp, n = 12)) %>% select(date, target, everything()) %>% drop_na() %>% arrange(date)
  
  #target variable 
  y = df_std %>% select(target)
  
  #predictors
  x = df_std %>% select(-target, -date)
  
  #lasso
  lasso_model <- rlasso(x, y, penalty=list(X.dependent.lambda=TRUE), post= FALSE)
  lasso_coeffs <- coef(lasso_model)
  
  # Convert to a data frame
  coeff_df <- data.frame(variable = names(lasso_coeffs), coefficient = as.numeric(lasso_coeffs))
  
  # Remove intercept and filter non-zero coefficients
  selected_vars <- coeff_df %>%
    filter(coefficient != 0, variable != "(Intercept)") %>% mutate(Importance = abs(coefficient)) %>%
    arrange(desc(Importance)) %>% select(variable) %>% pull(variable) %>% as.character()
  
  penalty <- lasso_model$lambda0
  return(penalty)
}
tune_lasso_h12(df)

