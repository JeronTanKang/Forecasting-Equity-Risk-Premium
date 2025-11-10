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
file_path = "../data/raw_predictors.xlsx"
df = read_excel(file_path)
df_diff = df %>% mutate(date = as.Date(paste0(sprintf("%06d", yyyymm), "01"), "%Y%m%d"))%>% select(-yyyymm) %>%
  mutate(erp = ret - Rfree) %>% select(date, erp, everything())

model_dict <- list(
  ret = "constant",
  Rfree = "trend",
  d12 = "trend",
  e12 = "trend",
  BAA = "trend",
  lty = "trend",
  d_p = "constant",
  d_y = "constant",
  e_p = "constant",
  d_e = "constant",
  tms = "constant",
  dfy = "constant",
  dfr = "constant",
  svar = "constant",
  infl = "constant",
  AAA = "trend",
  tbl = "trend",
  b_m = "constant",
  ltr = "constant",
  corpr = "constant", 
  lzrt = "constant",
  wtexas = "constant",
  dtoy = "constant",
  dtoat = "constant",
  avgcor = "constant",
  skvw = "constant",
  tail = "constant",
  rdsp = "constant",
  ntis = "trend",
  ygap = "constant",
  ndrbl = "constant",
  rsvix  = "constant",
  vrp = "constant",
  impvar = "constant",
  Volume = "trend",
  MA_spread_1_9 = "constant",
  MA_spread_1_12 = "constant",
  MA_spread_2_9 = "constant",
  MA_spread_2_12 = "constant",
  MA_spread_3_9 = "constant",
  MA_spread_3_12 = "constant"
  
)


#make the rest of the indicators stationary if they are not
make_stationary_dfgls <- function(df, model_dict, max_lags = 4, max_differences = 2) {
  
  df_stationary <- df  #copy of original dataset
  differencing_log <- list()  #list to store differencing count
  
  for (col in colnames(df)) {
    if (col != "date" && col %in% names(model_dict)) {  #exclude time column
      
      #ensure column is numeric before testing
      if (is.numeric(df[[col]])) {
        
        # Get model type from dictionary
        model_type <- model_dict[[col]]
        
        # Initialize differencing count
        differencing_count <- 0
        stationarity_achieved <- FALSE
        
        #run DF-GLS test
        while (!stationarity_achieved && differencing_count < max_differences) {
          
          # Ensure no missing values before testing
          test_data <- df_stationary[[col]]
          
          # Run DF-GLS test on the cleaned data
          test <- ur.ers(test_data, type = "DF-GLS", model = model_type, lag.max = max_lags)
          summary(test)
          # tstat and critval
          test_stat <- test@teststat
          critical_value <- test@cval[3] #use 10% significance level
          
          if (test_stat > critical_value) {  # Non-stationary, apply differencing
            df_stationary[[col]] <- c(NA, diff(df_stationary[[col]]))
            differencing_count <- differencing_count + 1
          } else {
            stationarity_achieved <- TRUE
          }
        }
        
        #store the number of times differencing was applied
        differencing_log[[col]] <- differencing_count
      }
    }
  }
  
  
  #print differencing log at the end
  print(differencing_log)
  
  return(df_stationary)
}

df_stationary = make_stationary_dfgls(df_diff, model_dict = model_dict) %>% drop_na()

#helper to create lags
lag_features <- function(data, lags = 3) {
  data %>%
    mutate(across(
      .cols = -date,  # Exclude the date column
      .fns = list(
        lag1 = ~ lag(., 1),
        lag2 = ~ lag(., 2),
        lag3 = ~ lag(., 3)
      ),
      .names = "{.col}_{.fn}" 
    ))
}
df_lagged = lag_features(df_stationary) %>% arrange(desc(date)) %>% drop_na() %>% arrange(date) %>% select(-infl, -ndrbl, -e12, -e_p, -d_e)
df_train = df_lagged %>% filter(row_number() > 100) %>% arrange(date)
write.csv(df_train, "stationary_indicators.csv", row.names = FALSE)
write.csv(df_lagged, "full_df.csv", row.names = FALSE)



# function that plots the series to see if trend or constant
plot_stationarity <- function(df, date_col = "date", cols = NULL, add_trend = TRUE) {
  stopifnot(date_col %in% names(df))
  
  if (is.null(cols)) {
    cols <- names(df)[sapply(df, is.numeric)]
  }
  stopifnot(all(cols %in% names(df)))
  
  long <- df %>%
    select(all_of(c(date_col, cols))) %>%
    rename(date = !!date_col) %>%
    pivot_longer(-date, names_to = "series", values_to = "value") %>%
    arrange(date)
  
  p <- ggplot(long, aes(x = date, y = value)) +
    geom_line() +
    facet_wrap(~ series, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = NULL, title = "Visual stationarity check") +
    theme_minimal(base_size = 12)
  
  if (add_trend) p <- p + geom_smooth(method = "lm", se = FALSE)
  
  print(p)
}
plot_stationarity(df_diff, date_col = "date", cols = c('Rfree'))  
