# install.packages(c("readxl","dplyr","TTR","lubridate"))  # if needed
library(readr)
library(dplyr)
library(zoo)
library(lubridate)

# 1) Load your data (set the correct path)
df <- read_csv('/Users/kellianneng/Downloads/Telegram Desktop/SP500_monthly_MS_from_daily.csv') 

# Sort the data 
df <- df %>%
  mutate(Date = mdy(Date)) %>%
  arrange(Date)


# 2) MA rules (uses df consistently)
add_ma_rules <- function(df, price_col = "Close",
                         s_vals = c(1,2,3), l_vals = c(9,12)) {
  stopifnot(price_col %in% names(df))
  p <- df[[price_col]]
  wins <- sort(unique(c(s_vals, l_vals)))
  for (n in wins) {
    df[[paste0("MA_", n)]] <- rollmean(p, k = n, align = "right", fill = NA)
  }
  for (s in s_vals) for (l in l_vals) if (s < l) {
    s_col <- paste0("MA_", s); l_col <- paste0("MA_", l)
    df[[paste0("MA_sig_", s, "_", l)]] <- ifelse(
      is.na(df[[s_col]]) | is.na(df[[l_col]]), NA_integer_,
      as.integer(df[[s_col]] >= df[[l_col]])
    )
    df[[paste0("MA_spread_", s, "_", l)]] <- df[[s_col]] - df[[l_col]]
  }
  df
}

## Adding Moving Average Tech Indicator into df
df <- add_ma_rules(df, price_col = "Close", s_vals = c(1,2,3), l_vals = c(9,12))

## Adding momentum = Comparing today's pice to the price m months ago using monthly Close
add_momentum <- function(df, price_col = "Close", m_vals = c(9,12)) {
  stopifnot(price_col %in% names(df))
  p <- df[[price_col]]
  for (m in m_vals) {
    df[[paste0("MOM_", m)]] <- p / dplyr::lag(p, m) - 1
    df[[paste0("MOM_sig_", m)]] <- dplyr::case_when(
      is.na(dplyr::lag(p, m)) ~ NA_integer_,
      TRUE ~ as.integer(p >= dplyr::lag(p, m))
    )
  }
  df
}

##Adding Momentum Tech Indicator into df 
df <- add_momentum(df, price_col = "Close", m_vals = c(9,12))


##Addig On-Balance Volume (OBV)
add_obv_rules <- function(df,
                          price_col = "Close",
                          vol_col   = "Volume",
                          s_vals = c(1,2,3),
                          l_vals = c(9,12)) {
  stopifnot(all(c(price_col, vol_col) %in% names(df)))
  
  p <- df[[price_col]]
  v <- df[[vol_col]]
  
  # D_k = +1 if P_t - P_{t-1} >= 0, else -1; 0 for the first row
  # D_k = +1 if price rises or stays, else -1 
  D  <- ifelse(is.na(dplyr::lag(p)), 0L,
               ifelse(p - dplyr::lag(p) >= 0, 1L, -1L))
  
  # OBV_t = cumulative sum of VOL_k * D_k
  df$OBV <- cumsum(v * D)
  
  # rolling MAs of OBV (right-aligned, no look-ahead)
  wins <- sort(unique(c(s_vals, l_vals)))
  for (n in wins) {
    df[[paste0("MA_OBV_", n)]] <- rollmean(df$OBV, k = n, align = "right", fill = NA)
  }
  
  # signals: 1 if MA_OBV_s >= MA_OBV_l, else 0 (NA during warm-up)
  for (s in s_vals) for (l in l_vals) if (s < l) {
    s_col <- paste0("MA_OBV_", s)
    l_col <- paste0("MA_OBV_", l)
    sig   <- paste0("OBV_sig_", s, "_", l)
    
    df[[sig]] <- ifelse(
      is.na(df[[s_col]]) | is.na(df[[l_col]]),
      NA_integer_,
      as.integer(df[[s_col]] >= df[[l_col]])
    )
    
    # optional continuous strength feature
    df[[paste0("OBV_spread_", s, "_", l)]] <- df[[s_col]] - df[[l_col]]
  }
  
  df
}


# Add to your existing df (already sorted by Date)
df <- add_obv_rules(df, price_col = "Close", vol_col = "Volume",
                    s_vals = c(1,2,3), l_vals = c(9,12))

print(df)

library(openxlsx)

write.xlsx(df, "/Users/kellianneng/Downloads/SP500_with_technical_indicators.xlsx", overwrite = TRUE)
