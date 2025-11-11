# Load necessary libraries
library(ggplot2)
library(dplyr)
library(randomForest)

# Load the data
df = read.csv("../data/full_df.csv") %>% mutate(date = as.Date(date, format = "%m/%d/%y"))

# Define forecast horizon
h_values <- 1

# Initialize lists to store the results of OOB MSE for RF and Bagging models
results_rf <- list()
results_bagging <- list()
  
# Preprocess the data: Standardize predictors, arrange by date, and create target (ERP)
df_std <- df %>% 
  mutate(across(-c(date, erp), ~ as.numeric(scale(.)))) %>%  
  arrange(date) %>% 
  mutate(target = lead(erp, n = h)) %>% 
  select(date, target, everything())

df_forecast <- df_std %>% slice(n() - h + 1)
df_train <- df_std %>% slice(1:(n() - h))

feats <- setdiff(names(df_std), c("date", "target"))
x_train <- df_train[, feats, drop = FALSE]
y_train <- df_train$target

# Fit Random Forest and Bagging models
rf_fit <- randomForest(x = x_train, y = y_train, ntree = 5000, mtry = floor(ncol(x_train) / 3))
bagging_fit <- randomForest(x = x_train, y = y_train, ntree = 5000, mtry = ncol(x_train))

# Store OOB MSE results
results_rf[[paste0("h", h)]] <- rf_fit$mse
results_bagging[[paste0("h", h)]] <- bagging_fit$mse 

# Plot OOB MSE over number of trees for both models
xx = seq(1, 5000, 1) 
plot(xx, results_rf[[paste0("h", h)]], type = "l", col = "blue", 
     ylab = "OOB MSE", xlab = "Number of Trees", 
     main = paste("OOB Error Comparison for h =", h), 
     ylim = range(c(results_rf[[paste0("h", h)]], results_bagging[[paste0("h", h)]])))
lines(xx, results_bagging[[paste0("h", h)]], col = "red") 

legend("topright", legend = c("Random Forest", "Bagging"), col = c("blue", "red"), lty = 1)
