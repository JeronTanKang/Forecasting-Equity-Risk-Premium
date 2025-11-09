# ---- libs ----
library(readxl)
library(dplyr)
library(sandwich)   # NeweyWest
library(tools)      # file_path_sans_ext
library(urca)
library(knitr)
library(kableExtra)

# ---- helpers ----
load_and_tag <- function(path, tag) {
  read_excel(path) %>%
    mutate(date = as.Date(date, format = "%d/%m/%y"))%>%
    select(date, residual_col_name) %>%
    rename(!!tag := residual_col_name) %>%
    arrange(date)
}
# --- df gls --- 
apply_dfgls_constant <- function(x) {
  x <- as.numeric(x); x <- x[is.finite(x)]
  
  lagmax <- max(1L, floor(length(x)^(1/3)))
  
  # Compute DF-GLS with an intercept
  er <- urca::ur.ers(x, type = "DF-GLS", model = "constant", lag.max = lagmax)
  
  # Extract single test statistic
  stat <- as.numeric(er@teststat[1])
  
  # robust 5% CV extraction
  cv <- er@cval
  get_cv5 <- function(cv) {
    if (is.matrix(cv)) {
      cn <- colnames(cv)
      j <- which(cn %in% c("5pct","5%"))
      if (length(j) == 0) j <- which(grepl("5", cn, ignore.case = TRUE))[1]
      if (is.na(j) && ncol(cv) >= 2) j <- 2
      if (is.na(j)) return(NA_real_)
      return(as.numeric(cv[1, j]))
    } else {
      nm <- names(cv)
      if (is.null(nm) || length(nm) == 0) {
        return(if (length(cv) >= 2) as.numeric(cv[2]) else NA_real_)
      }
      j <- which(nm %in% c("5pct","5%"))
      if (length(j) == 0) j <- which(grepl("5", nm, ignore.case = TRUE))[1]
      if (is.na(j) && length(cv) >= 2) j <- 2
      if (is.na(j)) return(NA_real_)
      return(as.numeric(cv[[j]]))
    }
  }
  crit5 <- get_cv5(cv)
  
  list(
    stat     = stat,
    crit5    = crit5,
    # If stat < crit5, reject unit root
    decision = if (stat < crit5) "stationary (reject H0)" else "non-stationary (fail to reject H0)"
  )
}



########################################################################################################
# Diebold-Mariano Test comparing 2 models' MSE 
# Goal: Quick check also if loss differential (d) is stationary 
########################################################################################################
dm_test_hac_regression <- function(e1, e2) {
  e1 <- as.numeric(e1); e2 <- as.numeric(e2)
  keep <- is.finite(e1) & is.finite(e2)
  e1 <- e1[keep]; e2 <- e2[keep]
  
  # compute d with model 1 = benchmark, model 2 = challenger model 
  # d is the loss differential 
  d <- e1^2 - e2^2
  
  #mad
  # d <- abs(e1) - abs(e2)
  
  # sanity-check stationary
  print(apply_dfgls_constant(d))
  
  T <- length(d)
  if (var(d) == 0) stop("Loss differential has zero variance.")
  lagNW <- round(T^(1/3))
  
  fit <- lm(d ~ 1)  # mean(d) with intercept-only
  Vnw <- NeweyWest(fit, lag = lagNW, prewhite = FALSE, adjust = TRUE)
  # Take HAC SE of the incercept 
  se  <- sqrt(Vnw[1,1])
  
  # dm statistic 
  dm  <- mean(d) / se
  p   <- 1 - pnorm(dm)  
  list(dm_stat = dm, p_value = p)
}

########################################################################################################
# Benchmark vs all models
# Goal: Compare prev_mean_benchmark_residuals (always model1) against every other model
########################################################################################################

benchmark_name <- "prev_mean_benchmark_residuals"

# --- DM test vs benchmark only; benchmark is always model1 ---
compare_vs_benchmark <- function(residual_list, benchmark_name) {
  stopifnot(benchmark_name %in% names(residual_list))
  others <- setdiff(names(residual_list), benchmark_name)
  
  rows <- lapply(others, function(m2) {
    pair <- dplyr::inner_join(residual_list[[benchmark_name]], residual_list[[m2]], by = "date")
    ans  <- dm_test_hac_regression(pair[[benchmark_name]], pair[[m2]])  # model1=benchmark, model2=challenger
    data.frame(
      model1 = benchmark_name,
      model2 = m2,
      DM_statistic = signif(ans$dm_stat, 3),
      p_value      = signif(ans$p_value, 3),
      Significant = ans$p_value < 0.05  # one-sided: challenger better if TRUE
    )
  })
  
  dplyr::bind_rows(rows)
}

# ---- All files ----
model_files <- c(
"../data/prev_mean_benchmark_residuals.xlsx",
"../data/pls_h1_residuals.xlsx",  
"../data/pls_h3_residuals.xlsx",
"../data/pls_h6_residuals.xlsx",
"../data/pls_h12_residuals.xlsx",
"../data/elnet_h1_residuals.xlsx",
"../data/elnet_h3_residuals.xlsx",
"../data/elnet_h6_residuals.xlsx",    
"../data/elnet_h12_residuals.xlsx", 
"../data/ridge_h1_residuals.xlsx",
"../data/ridge_h3_residuals.xlsx",
"../data/ridge_h6_residuals.xlsx",    
"../data/ridge_h12_residuals.xlsx", 
"../data/rf_h1_residuals.xlsx",
"../data/rf_h3_residuals.xlsx",
"../data/rf_h6_residuals.xlsx",    
"../data/rf_h12_residuals.xlsx",     
"../data/lasso_v2_h1_residuals.xlsx",
"../data/lasso_v2_h3_residuals.xlsx",
"../data/lasso_v2_h6_residuals.xlsx",    
"../data/lasso_v2_h12_residuals.xlsx",    
"../data/ridgeless_h1_residuals.xlsx",
"../data/ridgeless_h3_residuals.xlsx",
"../data/ridgeless_h6_residuals.xlsx",    
"../data/ridgeless_h12_residuals.xlsx", 
"../data/lasso_v1_h1_residuals.xlsx",
"../data/lasso_v1_h3_residuals.xlsx",
"../data/lasso_v1_h6_residuals.xlsx",
"../data/lasso_v1_h12_residuals.xlsx",
"../data/xgb_h1_residuals.xlsx",
"../data/xgb_h3_residuals.xlsx",
"../data/xgb_h6_residuals.xlsx",
"../data/xgb_h12_residuals.xlsx"
)

model_labels <- file_path_sans_ext(basename(model_files))

# load and tag each sheet
residual_data <- setNames(
  lapply(seq_along(model_files), \(k) load_and_tag(model_files[k], model_labels[k])),
  model_labels
)


# --- compare all lasso_v1_* vs lasso_v2_* with v1 fixed as model1 ---
compare_all_lasso_v1_vs_v2 <- function(residual_list) {
  nms <- names(residual_list)
  
  v1 <- nms[grepl("^lasso_v1_.*_residuals$", nms)]
  v2 <- sub("^lasso_v1_", "lasso_v2_", v1)
  keep <- v2 %in% nms
  v1 <- v1[keep]; v2 <- v2[keep]
  
  rows <- lapply(seq_along(v1), function(i) {
    m1 <- v1[i]; m2 <- v2[i]
    pair <- dplyr::inner_join(residual_list[[m1]], residual_list[[m2]], by = "date") |>
      dplyr::arrange(date)
    if (nrow(pair) < 10L) {
      return(data.frame(
        model1 = m1, model2 = m2,
        DM_statistic = NA_real_, p_value = NA_real_, Significant = NA
      ))
    }
    
    ans <- dm_test_hac_regression(pair[[m1]], pair[[m2]])  # H1: v2 MSE < v1 MSE
    data.frame(
      model1 = m1, model2 = m2,
      DM_statistic = signif(ans$dm_stat, 3),
      p_value      = signif(ans$p_value, 3),
      Significant = ans$p_value < 0.05
    )
  })
  
  dplyr::bind_rows(rows)
}


# Execution
# Prev_mean vs other models 
bench_results <- compare_vs_benchmark(residual_data, benchmark_name)

# Lasso v1 vs Lasso v2 
lasso_results <- compare_all_lasso_v1_vs_v2(residual_data)

# Merging 
final_results <- dplyr::bind_rows(bench_results, lasso_results)

print(final_results)
