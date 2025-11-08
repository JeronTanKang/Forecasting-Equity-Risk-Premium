#Load Packages 
library(readr)
library(dplyr)
library(pls)
library(ggplot2)
library(lubridate)

file_path = "../data/stationary_indicators.csv"
df = read.csv(file_path)

# 1. sort by date ascending
df <- df %>% 
  arrange(date)

mu_hat = mean(df$erp,na.rm = TRUE)
print(mu_hat) 

#0.005817197
