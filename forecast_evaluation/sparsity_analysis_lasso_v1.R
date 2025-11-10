
library(ggplot2)
library(dplyr)
library(rpart)
library(glmnet)
library(pls)
library(tidyr)
library(hdm)
# Load data
df = read.csv("../data/full_df.csv") %>% mutate(date = as.Date(date, format = "%m/%d/%y")) 

#change this h to 1/3/6/12 for which ever step 
h = 1

tech_indicators <- c(
  "MA_sig_1_9", "MA_spread_1_9", "MA_sig_1_12", "MA_spread_1_12",
  "MA_sig_2_9", "MA_spread_2_9", "MA_sig_2_12", "MA_spread_2_12",
  "MA_sig_3_9", "MA_spread_3_9", "MA_sig_3_12", "MA_spread_3_12",
  "MOM_sig_9", "MOM_sig_12",
  "OBV_sig_1_9", "OBV_spread_1_9", "OBV_sig_1_12", "OBV_spread_1_12",
  "OBV_sig_2_9", "OBV_spread_2_9", "OBV_sig_2_12", "OBV_spread_2_12",
  "OBV_sig_3_9", "OBV_spread_3_9", "OBV_sig_3_12", "OBV_spread_3_12"
)


model <- function(df, h){
  df_std = df %>% mutate(across(-c(date, erp), ~ as.numeric(scale(.)))) %>% arrange(date)
  df_std = df_std %>% mutate(target = lead(erp, n = h)) %>% select(date, target, everything())
  df_train = df_std %>% slice(1:(n() - h))
  
  #run lasso to find out which indicators are not shrunk to 0
  y = df_train %>% select(target)
  
  #predictors
  x = df_train %>% select(-target, -date)
  
  
  #lasso
  lasso_model <- rlasso(x, y, post= FALSE)
  lasso_coeffs <- coef(lasso_model)
  
  # Convert to a data frame
  coeff_df <- data.frame(variable = names(lasso_coeffs), coefficient = as.numeric(lasso_coeffs))
  
  # Get the names of selected variables
  selected_vars <- coeff_df %>%
    filter(coefficient != 0, variable != "(Intercept)") %>%
    pull(variable) %>% 
    as.character()
  
  if (length(selected_vars) == 0) {
    return(list(tech = 0, other = 0))
  }
  
  #remove the _lag naming stuff at the back
  base_names <- sub("_lag[0-9]+$", "", selected_vars)
  
  #see if got tech indicators used
  is_tech <- base_names %in% tech_indicators
  
  count_tech <- sum(is_tech)
  count_other <- sum(!is_tech)
  
  #return the counts as a list
  return(list(tech = count_tech, other = count_other))
  

}

WINDOW_SIZE = 100 
model_name = 'model_name' 


run_recursive_window <- function(df, model_name) {
  
  date_col <- tail(df[[1]], WINDOW_SIZE) 
  T_total <- nrow(df)
  
  preds_df <- data.frame(
    date = date_col,              
    count_technical = rep(NA_integer_, WINDOW_SIZE), 
    count_other = rep(NA_integer_, WINDOW_SIZE)     
  )
  
  # 3. fixed size windows
  for (t in seq(T_total - WINDOW_SIZE + 1, T_total)) { # t goes from 229 to 328
    trim_index <- t - 1
    test_index <- t - (T_total - WINDOW_SIZE + 1) # test_index goes from 0 to 99
    df_trimmed <- df[0+test_index:trim_index, , drop = FALSE]
    

    counts_list <- model(df_trimmed, h)
    
    # Store the counts from the list
    preds_df$count_technical[test_index + 1] <- counts_list$tech
    preds_df$count_other[test_index + 1] <- counts_list$other
    preds_df$total[test_index + 1] <-counts_list$tech + counts_list$other
    
  }
  
  return(list(
    preds_df = preds_df
  ))
}


res <- run_recursive_window(df, model_name)
res_list <- res$preds_df
res_df <- data.frame(date = res_list$date, technical = res_list$count_technical, total = res_list$total)



ggplot(res_df, aes(x = date)) +
  geom_line(aes(y = technical, color = "technical"), linewidth = 1) +
  geom_line(aes(y = total, color = "total"), linewidth = 1) +
  geom_vline(
    xintercept = as.Date("2020-03-01"), 
    linetype = "dashed", 
    color = "red", 
    linewidth = 1
  ) +
  labs(
    title = paste("Sparsity Analysis (h", h, ")", sep = ""),
    x = "Date",
    y = "No.of indicators",
    color = "Legend"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("technical" = "blue", "total" = "grey"))

