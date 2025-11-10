### This code plots out the coefficients of each indicators selected by Lasso_v1 over the forecast window.
### Specify h=1/3/6/12 at line 20

library(ggplot2)
library(dplyr)
library(rpart)
library(glmnet)
library(pls)
library(tidyr)
library(hdm)
library(ggrepel)
# Load data




df = read.csv("../data/full_df.csv") %>% mutate(date = as.Date(date, format = "%m/%d/%y")) 


h = 1


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
    filter(coefficient != 0, variable != "(Intercept)")
  
  all_vars <- colnames(x)
  beta <- setNames(rep(0, length(all_vars)), all_vars)
  nz <- intersect(selected_vars$variable, all_vars)
  if (length(nz) > 0) {
    beta[nz] <- selected_vars$coefficient[match(nz, selected_vars$variable)]
  }
  return(beta)
  
  
}

WINDOW_SIZE = 100 
model_name = 'model_name' 


run_recursive_window <- function(df, model_name) {
  
  date_col <- tail(df[[1]], WINDOW_SIZE) 
  T_total <- nrow(df)
  
  preds_df <- data.frame(
    date = as.Date(character()),
    variable = character(),
    coefficient = numeric(),
    stringsAsFactors = FALSE
  )
  

  for (t in seq(T_total - WINDOW_SIZE + 1, T_total)) { # t: 229..328
    trim_index <- t - 1
    test_index <- t - (T_total - WINDOW_SIZE + 1)       # 0..99
    
    start_idx <- 1 + test_index
    end_idx   <- trim_index
    if (end_idx < start_idx) next
    
    df_trimmed <- df[start_idx:end_idx, , drop = FALSE]
    
    selected_indicators <- model(df_trimmed, h)         
    
    window_date <- df_trimmed$date[nrow(df_trimmed)]     
    tmp <- data.frame(
      date = rep(window_date, length(selected_indicators)),
      variable = names(selected_indicators),
      coefficient = as.numeric(selected_indicators),
      stringsAsFactors = FALSE
    )
    
    preds_df <- rbind(preds_df, tmp)
  }
  
  return(list(preds_df = preds_df))
}

res <- run_recursive_window(df, model_name)

#filter away those that never got chosen
res_filtered <- res
res_filtered$preds_df <- res$preds_df %>%
  dplyr::group_by(variable) %>%
  dplyr::filter(any(coefficient != 0)) %>%
  dplyr::ungroup()

# top 3 indicator w highest coefficient
top_3_indicators <- res_filtered$preds_df %>%
  group_by(variable) %>%
  summarize(max_coef = max(abs(coefficient), na.rm = TRUE)) %>%
  slice_max(order_by = max_coef, n = 3)


label_df <- res_filtered$preds_df %>%
  group_by(variable) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup()


ggplot(res_filtered$preds_df, aes(date, coefficient, color = variable)) +
  geom_line() +
  geom_point(data = label_df, size = 1.2) +
  geom_label_repel(
    data = label_df,
    aes(label = variable),
    direction = "y", label.size = 0.05,
    min.segment.length = 0,
    size = 3,         # decrease label size
    max.overlaps = 10,
    label.r = 0# increase max overlaps (default is 10)
  ) +
  scale_x_date(expand = expansion(mult = c(0.02, 0.08))) +
  labs(x = NULL, y = "Coefficient", color = "Indicator" ,title = paste("Coefficient Analysis (h", h, ")", sep = "")) +
  theme_minimal() +
  theme(legend.position = "none")


