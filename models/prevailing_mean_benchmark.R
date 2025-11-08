#Load Packages 
library(readr)
library(dplyr)
library(writexl)

file_path = "../data/stationary_indicators.csv"
df = read.csv(file_path)

# 1. sort by date ascending
df <- df %>% 
  arrange(date)

# 2. Calculate Prevailing Mean 
mu_hat = mean(df$erp,na.rm = TRUE)
print(mu_hat) 
#0.005817197


####################################
# Residuals of benchmark model for 
# DM Test 
####################################
file_path_1 = "../data/full_df.csv"
df1 = read.csv(file_path_1) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%y")) 

prevailing_mean = 0.005817197   # Prevailing mean is hardcoded 
cutoff <- as.Date("2015-05-01") # Test set 

# Only have date + residual_col_name 
df2 = df1 %>%
  mutate(residual_col_name = erp - prevailing_mean) %>%
  filter(date >= cutoff) %>% 
  select(date, residual_col_name)

# Save Residuals into excel for dm test 
write_xlsx(df2, "../data/prev_mean_benchmark_residuals.xlsx")
