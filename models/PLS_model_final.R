#Load Packages 
library(readr)
library(dplyr)
library(pls)
library(ggplot2)
library(lubridate)

############################################################
# 0. Load data ONCE into df and sort chronologically
############################################################

file_path = "../data/stationary_indicators.csv"
df = read.csv(file_path)

############################################################
# 1. Choosing optimal components via expanding CV
############################################################
tune_pls_h1 <- function(df, max_comp = 10, min_train = 100) {
  
  # 1. sort by date ascending
  df <- df %>%
    arrange(date)
  
  # 2. build Y = ERP at t+1
  df$Y <- dplyr::lead(df$erp, 1)
  
  # 3. drop last row (Y is NA there)
  df <- df %>% filter(!is.na(Y))
  
  # 4. Build X matrix of predictors available at time t 
  predictor_cols <- setdiff(colnames(df), c("date", "Y"))
  X_all <- df[, predictor_cols]
  Y_all <- df$Y
  
  # Total rows
  T_total <- nrow(df)
  
  # Check if we have enough history
  if (T_total < min_train + 1) {
    stop("Not enough rows for the requested min_train")
  }
  
  start_forecast_idx <- min_train + 1
  
  # Container for RMSE for each component 
  rmse_vec <- rep(NA_real_, max_comp)
  
  # 7. Loop over candidate number of comps 
  for (K in 1:max_comp) {
    preds <- c()
    actual <- c()
    
    # expanding-window walk forward
    for (t_idx in start_forecast_idx:T_total) {
      # training window is rows 1 .. (t_idx-1)
      train_idx <- 1:(t_idx - 1)
      
      X_train <- X_all[train_idx, , drop = FALSE]
      Y_train <- Y_all[train_idx]
      
      # fit PLS with fixed K comps
      pls_fit <- plsr(Y_train ~ .,
                      data   = as.data.frame(X_train),
                      ncomp  = K,
                      scale  = TRUE,
                      method = "oscorespls")
      # forecast row t_idx
      X_next <- X_all[t_idx, , drop = FALSE]
      
      yhat <- predict(pls_fit,
                      newdata = as.data.frame(X_next),
                      ncomp   = K)
      
      preds  <- c(preds,  as.numeric(yhat))
      actual <- c(actual, Y_all[t_idx])
    }
    
    # compute RMSE for this K
    err_sq <- (actual - preds)^2
    rmse_vec[K] <- sqrt(mean(err_sq))
  }
  
  best_K <- which.min(rmse_vec)
  
  return(list(
    best_ncomp = best_K, 
    rmse_by_comp = rmse_vec
  ))
}

res_h1 <- tune_pls_h1(df, max_comp = 10, min_train = 100) 
h1_comp <- res_h1$best_ncomp
res_h1$rmse_by_comp
print(h1_comp) # 1 component 

###################################################
# Fit  PLS on all training data using h1_comp
# to retrieve frozen loadings 
###################################################
build_pls_h1 <- function(df, h1_comp, save_path = "../code/pls_structure_h1.rds") {
  
  # 1. align data for horizon h=3 (target is ERP at t+3)
  train_df <- df %>%
    mutate(date = as.Date(date)) %>%
    arrange(date) %>%
    mutate(Y = dplyr::lead(erp, 1)) %>%
    filter(!is.na(Y))
  
  # 2. get predictor columns (everything except date and Y)
  predictor_cols <- setdiff(colnames(train_df), c("date", "Y"))
  
  X_train_full <- train_df[, predictor_cols, drop = FALSE]
  Y_train_full <- train_df$Y
  
  # 3. compute training means / sds for each predictor
  X_means <- sapply(X_train_full, mean, na.rm = TRUE)
  X_sds   <- sapply(X_train_full, sd,   na.rm = TRUE)
  
  # avoid divide-by-zero if any predictor is constant
  X_sds[X_sds == 0] <- 1
  
  # 4. standardize predictors using those training stats
  X_train_std <- scale(X_train_full, center = X_means, scale = X_sds)
  X_train_std <- as.data.frame(X_train_std)
  
  # 5. fit PLS on standardized X to predict Y(t+3)
  pls_fit_full <- plsr(
    Y_train_full ~ .,
    data   = X_train_std,
    ncomp  = h1_comp,
    scale  = FALSE,          # we already standardized manually
    method = "oscorespls"
  )
  
  # 6. extract frozen loadings (weights for components)
  W_full <- pls_fit_full$loading.weights[, 1:h1_comp, drop = FALSE]
  # rows of W_full line up with predictor_cols order
  
  # 7. package structure we’ll need at forecast time
  pls_structure <- list(
    h1_comp        = h1_comp,        # number of components for h=3
    predictor_cols = predictor_cols, # column order lock-in
    X_means        = X_means,        # training means for scaling new data
    X_sds          = X_sds,          # training sds for scaling new data
    W_full         = W_full          # frozen loadings matrix
  )
  
  # 8. save to disk so h3_model() can load it later
  saveRDS(pls_structure, file = save_path)
  message("Saved ", save_path)
  
  # 9. also return it so you can inspect immediately if you want
  return(pls_structure)
}

pls_h1_struct <- build_pls_h1(df, h1_comp)

######################
### Predicting 3 step
#######################

tune_pls_h3 <- function(df, max_comp = 10, min_train = 100) {
  
  # 1. sort by date ascending
  df <- df %>%
    arrange(date)
  
  # 2. build Y = ERP at t+3
  df$Y <- dplyr::lead(df$erp, 3)
  
  # 3. drop last row (Y is NA there)
  df <- df %>% filter(!is.na(Y))
  
  # 4. Build X matrix of predictors available at time t 
  predictor_cols <- setdiff(colnames(df), c("date", "Y"))
  X_all <- df[, predictor_cols]
  Y_all <- df$Y
  
  # Total rows
  T_total <- nrow(df)
  
  # Check if we have enough history
  if (T_total < min_train + 1) {
    stop("Not enough rows for the requested min_train")
  }
  
  start_forecast_idx <- min_train + 1
  
  # Container for RMSE for each component 
  rmse_vec <- rep(NA_real_, max_comp)
  
  # 7. Loop over candidate number of comps 
  for (K in 1:max_comp) {
    preds <- c()
    actual <- c()
    
    # expanding-window walk forward
    for (t_idx in start_forecast_idx:T_total) {
      # training window is rows 1 .. (t_idx-1)
      train_idx <- 1:(t_idx - 1)
      
      X_train <- X_all[train_idx, , drop = FALSE]
      Y_train <- Y_all[train_idx]
      
      # fit PLS with fixed K comps
      pls_fit <- plsr(Y_train ~ .,
                      data   = as.data.frame(X_train),
                      ncomp  = K,
                      scale  = TRUE,
                      method = "oscorespls")
      # forecast row t_idx
      X_next <- X_all[t_idx, , drop = FALSE]
      
      yhat <- predict(pls_fit,
                      newdata = as.data.frame(X_next),
                      ncomp   = K)
      
      preds  <- c(preds,  as.numeric(yhat))
      actual <- c(actual, Y_all[t_idx])
    }
    
    # compute RMSE for this K
    err_sq <- (actual - preds)^2
    rmse_vec[K] <- sqrt(mean(err_sq))
  }
  
  best_K <- which.min(rmse_vec)
  
  return(list(
    best_ncomp = best_K, 
    rmse_by_comp = rmse_vec
  ))
}

res_h3 <- tune_pls_h3(df, max_comp = 10, min_train = 100) 
h3_comp <- res_h3$best_ncomp
res_h3$rmse_by_comp
print(h3_comp) # 2 Components 

###################################################
# Fit  PLS on all training data using h3_comp
# to retrieve frozen loadings 
###################################################

build_pls_h3 <- function(df, h3_comp, save_path = "../code/pls_structure_h3.rds") {
  
  # 1. align data for horizon h=3 (target is ERP at t+3)
  train_df <- df %>%
    mutate(date = as.Date(date)) %>%
    arrange(date) %>%
    mutate(Y = dplyr::lead(erp, 3)) %>%
    filter(!is.na(Y))
  
  # 2. get predictor columns (everything except date and Y)
  predictor_cols <- setdiff(colnames(train_df), c("date", "Y"))
  
  X_train_full <- train_df[, predictor_cols, drop = FALSE]
  Y_train_full <- train_df$Y
  
  # 3. compute training means / sds for each predictor
  X_means <- sapply(X_train_full, mean, na.rm = TRUE)
  X_sds   <- sapply(X_train_full, sd,   na.rm = TRUE)
  
  # avoid divide-by-zero if any predictor is constant
  X_sds[X_sds == 0] <- 1
  
  # 4. standardize predictors using those training stats
  X_train_std <- scale(X_train_full, center = X_means, scale = X_sds)
  X_train_std <- as.data.frame(X_train_std)
  
  # 5. fit PLS on standardized X to predict Y(t+3)
  pls_fit_full <- plsr(
    Y_train_full ~ .,
    data   = X_train_std,
    ncomp  = h3_comp,
    scale  = FALSE,          # we already standardized manually
    method = "oscorespls"
  )
  
  # 6. extract frozen loadings (weights for components)
  W_full <- pls_fit_full$loading.weights[, 1:h3_comp, drop = FALSE]
  # rows of W_full line up with predictor_cols order
  
  # 7. package structure we’ll need at forecast time
  pls_structure <- list(
    h3_comp        = h3_comp,        # number of components for h=3
    predictor_cols = predictor_cols, # column order lock-in
    X_means        = X_means,        # training means for scaling new data
    X_sds          = X_sds,          # training sds for scaling new data
    W_full         = W_full          # frozen loadings matrix
  )
  
  # 8. save to disk so h3_model() can load it later
  saveRDS(pls_structure, file = save_path)
  message("Saved ", save_path)
  
  # 9. also return it so you can inspect immediately if you want
  return(pls_structure)
}

pls_h3_struct <- build_pls_h3(df, h3_comp)

######################
### Predicting 6 step
#######################

tune_pls_h6 <- function(df, max_comp = 10, min_train = 100) {
  
  # 1. sort by date ascending
  df <- df %>%
    arrange(date)
  
  # 2. build Y = ERP at t+6
  df$Y <- dplyr::lead(df$erp, 6)
  
  # 3. drop last row (Y is NA there)
  df <- df %>% filter(!is.na(Y))
  
  # 4. Build X matrix of predictors available at time t 
  predictor_cols <- setdiff(colnames(df), c("date", "Y"))
  X_all <- df[, predictor_cols]
  Y_all <- df$Y
  
  # Total rows
  T_total <- nrow(df)
  
  # Check if we have enough history
  if (T_total < min_train + 1) {
    stop("Not enough rows for the requested min_train")
  }
  
  start_forecast_idx <- min_train + 1
  
  # Container for RMSE for each component 
  rmse_vec <- rep(NA_real_, max_comp)
  
  # 7. Loop over candidate number of comps 
  for (K in 1:max_comp) {
    preds <- c()
    actual <- c()
    
    # expanding-window walk forward
    for (t_idx in start_forecast_idx:T_total) {
      # training window is rows 1 .. (t_idx-1)
      train_idx <- 1:(t_idx - 1)
      
      X_train <- X_all[train_idx, , drop = FALSE]
      Y_train <- Y_all[train_idx]
      
      # fit PLS with fixed K comps
      pls_fit <- plsr(Y_train ~ .,
                      data   = as.data.frame(X_train),
                      ncomp  = K,
                      scale  = TRUE,
                      method = "oscorespls")
      # forecast row t_idx
      X_next <- X_all[t_idx, , drop = FALSE]
      
      yhat <- predict(pls_fit,
                      newdata = as.data.frame(X_next),
                      ncomp   = K)
      
      preds  <- c(preds,  as.numeric(yhat))
      actual <- c(actual, Y_all[t_idx])
    }
    
    # compute RMSE for this K
    err_sq <- (actual - preds)^2
    rmse_vec[K] <- sqrt(mean(err_sq))
  }
  
  best_K <- which.min(rmse_vec)
  
  return(list(
    best_ncomp = best_K, 
    rmse_by_comp = rmse_vec
  ))
}

res_h6 <- tune_pls_h6(df, max_comp = 10, min_train = 100) 
h6_comp <- res_h6$best_ncomp
res_h6$rmse_by_comp
print(h6_comp) # 2 Components 

###################################################
# Fit  PLS on all training data using h6_comp
# to retrieve frozen loadings 
###################################################

build_pls_h6 <- function(df, h6_comp, save_path = "../code/pls_structure_h6.rds") {
  
  # 1. align data for horizon h=3 (target is ERP at t+6)
  train_df <- df %>%
    mutate(date = as.Date(date)) %>%
    arrange(date) %>%
    mutate(Y = dplyr::lead(erp, 6)) %>%
    filter(!is.na(Y))
  
  # 2. get predictor columns (everything except date and Y)
  predictor_cols <- setdiff(colnames(train_df), c("date", "Y"))
  
  X_train_full <- train_df[, predictor_cols, drop = FALSE]
  Y_train_full <- train_df$Y
  
  # 3. compute training means / sds for each predictor
  X_means <- sapply(X_train_full, mean, na.rm = TRUE)
  X_sds   <- sapply(X_train_full, sd,   na.rm = TRUE)
  
  # avoid divide-by-zero if any predictor is constant
  X_sds[X_sds == 0] <- 1
  
  # 4. standardize predictors using those training stats
  X_train_std <- scale(X_train_full, center = X_means, scale = X_sds)
  X_train_std <- as.data.frame(X_train_std)
  
  # 5. fit PLS on standardized X to predict Y(t+6)
  pls_fit_full <- plsr(
    Y_train_full ~ .,
    data   = X_train_std,
    ncomp  = h6_comp,
    scale  = FALSE,          # we already standardized manually
    method = "oscorespls"
  )
  
  # 6. extract frozen loadings (weights for components)
  W_full <- pls_fit_full$loading.weights[, 1:h6_comp, drop = FALSE]
  # rows of W_full line up with predictor_cols order
  
  # 7. package structure we’ll need at forecast time
  pls_structure <- list(
    h6_comp        = h6_comp,        # number of components for h=3
    predictor_cols = predictor_cols, # column order lock-in
    X_means        = X_means,        # training means for scaling new data
    X_sds          = X_sds,          # training sds for scaling new data
    W_full         = W_full          # frozen loadings matrix
  )
  
  # 8. save to disk so h6_model() can load it later
  saveRDS(pls_structure, file = save_path)
  message("Saved ", save_path)
  
  # 9. also return it so you can inspect immediately if you want
  return(pls_structure)
}

pls_h6_struct <- build_pls_h6(df, h6_comp)


######################
### Predicting 12 step
#######################

tune_pls_h12 <- function(df, max_comp = 10, min_train = 100) {
  
  # 1. sort by date ascending
  df <- df %>%
    arrange(date)
  
  # 2. build Y = ERP at t+12
  df$Y <- dplyr::lead(df$erp, 12)
  
  # 3. drop last row (Y is NA there)
  df <- df %>% filter(!is.na(Y))
  
  # 4. Build X matrix of predictors available at time t 
  predictor_cols <- setdiff(colnames(df), c("date", "Y"))
  X_all <- df[, predictor_cols]
  Y_all <- df$Y
  
  # Total rows
  T_total <- nrow(df)
  
  # Check if we have enough history
  if (T_total < min_train + 1) {
    stop("Not enough rows for the requested min_train")
  }
  
  start_forecast_idx <- min_train + 1
  
  # Container for RMSE for each component 
  rmse_vec <- rep(NA_real_, max_comp)
  
  # 7. Loop over candidate number of comps 
  for (K in 1:max_comp) {
    preds <- c()
    actual <- c()
    
    # expanding-window walk forward
    for (t_idx in start_forecast_idx:T_total) {
      # training window is rows 1 .. (t_idx-1)
      train_idx <- 1:(t_idx - 1)
      
      X_train <- X_all[train_idx, , drop = FALSE]
      Y_train <- Y_all[train_idx]
      
      # fit PLS with fixed K comps
      pls_fit <- plsr(Y_train ~ .,
                      data   = as.data.frame(X_train),
                      ncomp  = K,
                      scale  = TRUE,
                      method = "oscorespls")
      # forecast row t_idx
      X_next <- X_all[t_idx, , drop = FALSE]
      
      yhat <- predict(pls_fit,
                      newdata = as.data.frame(X_next),
                      ncomp   = K)
      
      preds  <- c(preds,  as.numeric(yhat))
      actual <- c(actual, Y_all[t_idx])
    }
    
    # compute RMSE for this K
    err_sq <- (actual - preds)^2
    rmse_vec[K] <- sqrt(mean(err_sq))
  }
  
  best_K <- which.min(rmse_vec)
  
  return(list(
    best_ncomp = best_K, 
    rmse_by_comp = rmse_vec
  ))
}

res_h12 <- tune_pls_h12(df, max_comp = 10, min_train = 100) 
h12_comp <- res_h12$best_ncomp
res_h12$rmse_by_comp
print(h12_comp) # 1 Components 


###################################################
# Fit  PLS on all training data using h12_comp
# to retrieve frozen loadings 
###################################################

build_pls_h12 <- function(df, h12_comp, save_path = "../code/pls_structure_h12.rds") {
  
  # 1. align data for horizon h=3 (target is ERP at t+12)
  train_df <- df %>%
    mutate(date = as.Date(date)) %>%
    arrange(date) %>%
    mutate(Y = dplyr::lead(erp, 12)) %>%
    filter(!is.na(Y))
  
  # 2. get predictor columns (everything except date and Y)
  predictor_cols <- setdiff(colnames(train_df), c("date", "Y"))
  
  X_train_full <- train_df[, predictor_cols, drop = FALSE]
  Y_train_full <- train_df$Y
  
  # 3. compute training means / sds for each predictor
  X_means <- sapply(X_train_full, mean, na.rm = TRUE)
  X_sds   <- sapply(X_train_full, sd,   na.rm = TRUE)
  
  # avoid divide-by-zero if any predictor is constant
  X_sds[X_sds == 0] <- 1
  
  # 4. standardize predictors using those training stats
  X_train_std <- scale(X_train_full, center = X_means, scale = X_sds)
  X_train_std <- as.data.frame(X_train_std)
  
  # 5. fit PLS on standardized X to predict Y(t+12)
  pls_fit_full <- plsr(
    Y_train_full ~ .,
    data   = X_train_std,
    ncomp  = h12_comp,
    scale  = FALSE,          # we already standardized manually
    method = "oscorespls"
  )
  
  # 6. extract frozen loadings (weights for components)
  W_full <- pls_fit_full$loading.weights[, 1:h12_comp, drop = FALSE]
  # rows of W_full line up with predictor_cols order
  
  # 7. package structure we’ll need at forecast time
  pls_structure <- list(
    h12_comp        = h12_comp,        # number of components for h=3
    predictor_cols = predictor_cols, # column order lock-in
    X_means        = X_means,        # training means for scaling new data
    X_sds          = X_sds,          # training sds for scaling new data
    W_full         = W_full          # frozen loadings matrix
  )
  
  # 8. save to disk so h6_model() can load it later
  saveRDS(pls_structure, file = save_path)
  message("Saved ", save_path)
  
  # 9. also return it so you can inspect immediately if you want
  return(pls_structure)
}

pls_h12_struct <- build_pls_h12(df, h12_comp)


############################################
# Making prediction model using the trained 
# model 
############################################

pls_model <- function(df, h) {
  # df: all data available up to "now"
  # h: forecast horizon in months ahead. allowed: 1,3,6,12
  # RETURNS: scalar forecast of ERP(t+h)
  
  # 0. choose which RDS to load based on h
  struct_path <- switch(
    as.character(h),
    "1"  = "../code/pls_structure_h1.rds",
    "3"  = "../code/pls_structure_h3.rds",
    "6"  = "../code/pls_structure_h6.rds",
    "12" = "../code/pls_structure_h12.rds",
    stop("Unsupported horizon h. Use 1,3,6,12.")
  )
  
  pls_structure <- readRDS(struct_path)
  
  # 1. unpack horizon-specific fields
  #    each RDS has different naming for the number of comps:
  #    h1_comp, h3_comp, h6_comp, h12_comp
  n_comp <- switch(
    as.character(h),
    "1"  = pls_structure$h1_comp,
    "3"  = pls_structure$h3_comp,
    "6"  = pls_structure$h6_comp,
    "12" = pls_structure$h12_comp
  )
  
  predictor_cols <- pls_structure$predictor_cols
  X_means        <- pls_structure$X_means
  X_sds          <- pls_structure$X_sds
  W_full         <- pls_structure$W_full   # [num_predictors x n_comp]
  
  # helper for standardization using frozen training stats
  standardize_like_training <- function(X_raw, means_vec, sds_vec) {
    X_mat       <- as.matrix(X_raw)
    X_centered  <- sweep(X_mat, 2, means_vec[colnames(X_raw)], FUN = "-")
    X_std       <- sweep(X_centered, 2, sds_vec[colnames(X_raw)], FUN = "/")
    X_std
  }
  
  # (1) sort chronologically and build Y = ERP_{t+h}
  df_sorted <- df %>%
    mutate(date = as.Date(date)) %>%
    arrange(date) %>%
    mutate(Y = dplyr::lead(erp, h))
  
  # (2) drop final NA Y (no realized ERP(t+h) yet)
  df_in <- df_sorted %>%
    filter(!is.na(Y))
  
  # must have at least 2 rows:
  # - rows 1:(n-1) to estimate beta
  # - row n to forecast
  if (nrow(df_in) < 2) {
    return(NA_real_)
  }
  
  # pull predictors in locked order
  X_all_raw <- df_in[, predictor_cols, drop = FALSE]
  
  # standardize with TRAINING stats (frozen means/sds)
  X_all_std <- standardize_like_training(
    X_raw     = X_all_raw,
    means_vec = X_means,
    sds_vec   = X_sds
  )
  
  # project onto frozen PLS loadings
  # Scores_all: [rows x n_comp]
  Scores_all <- X_all_std %*% W_full
  
  # (3) expanding-window regression of Y on the component scores
  Y_all    <- df_in$Y
  last_idx <- nrow(df_in)
  
  Scores_train   <- Scores_all[1:(last_idx - 1), , drop = FALSE]
  Y_train        <- Y_all[1:(last_idx - 1)]
  Score_forecast <- Scores_all[last_idx, , drop = FALSE]
  
  # build regression frame
  scores_df <- as.data.frame(Scores_train)
  colnames(scores_df) <- paste0("T", seq_len(n_comp))
  
  reg_df <- data.frame(
    Y = Y_train,
    scores_df
  )
  
  fit_beta <- lm(
    formula = as.formula(
      paste("Y ~", paste(colnames(scores_df), collapse = " + "))
    ),
    data = reg_df
  )
  
  last_scores <- as.data.frame(Score_forecast)
  colnames(last_scores) <- paste0("T", seq_len(n_comp))
  
  pred_next <- predict(fit_beta, newdata = last_scores)
  
  as.numeric(pred_next)
}

