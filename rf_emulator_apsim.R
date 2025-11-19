## =============================================================================
## Random forest emulator for APSIM crop yield and projections under multiple GCMs
##
## This script defines helper functions to:
##   1) Load APSIM output and set crop yield as 'yield'
##   2) Tune a random forest emulator using OOB RMSE
##   3) Fit the final emulator
##   4) Apply the emulator to new predictor data (e.g., future GCM scenarios)
##
## All data (APSIM outputs and predictor inputs) are assumed to be preprocessed.
## =============================================================================

library(ranger)

## -----------------------------------------------------------------------------
## Load APSIM output and set crop yield as response 'yield'
## -----------------------------------------------------------------------------
# path:      file path to APSIM training output (CSV)
# yield_col: column name in the APSIM file that stores crop yield (e.g., "Yield")
load_apsim_output <- function(path, yield_col = "Yield") {
  df <- read.csv(path)
  
  if (!yield_col %in% names(df)) {
    stop("Yield column '", yield_col, "' not found in APSIM output.")
  }
  
  # Copy APSIM yield into a standard response column 'yield'
  df$yield <- df[[yield_col]]
  
  # Optionally remove the original yield column if it has a different name
  if (yield_col != "yield") {
    df[[yield_col]] <- NULL
  }
  
  df
}

## -----------------------------------------------------------------------------
## Hyperparameter tuning using out-of-bag RMSE
## -----------------------------------------------------------------------------
tune_rf_emulator <- function(train_df,
                             response   = "yield",
                             mtry_grid  = seq(1, 9, by = 2),
                             ntree_grid = c(100, 200, 300, 500, 800, 1000),
                             seed       = 123) {
  set.seed(seed)
  
  if (!response %in% names(train_df)) {
    stop("Response variable '", response, "' not found in training data.")
  }
  
  grid <- expand.grid(mtry = mtry_grid, ntree = ntree_grid)
  grid$RMSE <- NA_real_
  
  formula <- as.formula(paste(response, "~ ."))
  y       <- train_df[[response]]
  
  for (i in seq_len(nrow(grid))) {
    fit <- ranger(
      formula,
      data       = train_df,
      mtry       = grid$mtry[i],
      num.trees  = grid$ntree[i],
      importance = "impurity",
      keep.inbag = TRUE
    )
    
    pred <- fit$predictions
    grid$RMSE[i] <- sqrt(mean((pred - y)^2, na.rm = TRUE))
  }
  
  best_idx <- which.min(grid$RMSE)
  
  list(
    grid = grid,                             # all (mtry, ntree, RMSE)
    best = grid[best_idx, , drop = FALSE]    # best combination
  )
}

## -----------------------------------------------------------------------------
## Fit final RF emulator with chosen hyperparameters
## -----------------------------------------------------------------------------
fit_rf_emulator <- function(train_df,
                            response  = "yield",
                            mtry,
                            ntree,
                            seed      = 123) {
  set.seed(seed)
  
  if (!response %in% names(train_df)) {
    stop("Response variable '", response, "' not found in training data.")
  }
  
  formula <- as.formula(paste(response, "~ ."))
  
  ranger(
    formula,
    data       = train_df,
    mtry       = mtry,
    num.trees  = ntree,
    importance = "impurity"
  )
}

## -----------------------------------------------------------------------------
## Apply emulator to new data
## -----------------------------------------------------------------------------
predict_rf_emulator <- function(model, new_df) {
  # Assumes new_df has the same predictor columns as the training data (except 'yield')
  predict(model, data = new_df)$predictions
}

## -----------------------------------------------------------------------------
## (Optional) Helper: project future yields for a list of GCMs
## -----------------------------------------------------------------------------
# This is just a convenience wrapper; you can also loop manually in your own script.
project_gcm_scenarios <- function(model,
                                  gcm_ids,
                                  input_dir,
                                  output_dir,
                                  input_prefix  = "future_",
                                  output_prefix = "yield_projection_") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (gcm in gcm_ids) {
    in_path <- file.path(input_dir, paste0(input_prefix, gcm, ".csv"))
    if (!file.exists(in_path)) {
      warning("Future input file not found for GCM: ", gcm, " (", in_path, ")")
      next
    }
    
    df <- read.csv(in_path)
    
    # Ensure no 'yield' column is accidentally used as a predictor
    if ("yield" %in% names(df)) {
      df$yield <- NULL
    }
    
    df$yield_emulator <- predict_rf_emulator(model, df)
    
    out_path <- file.path(output_dir, paste0(output_prefix, gcm, ".csv"))
    write.csv(df, out_path, row.names = FALSE)
  }
}

## =============================================================================
## Example workflow (commented out for code availability)
## =============================================================================
## Uncomment and adapt paths / settings to run.
##
## # 1. Load APSIM training data
## apsim_train <- load_apsim_output(
##   path      = "data/apsim_train.csv",
##   yield_col = "Yield"
## )
##
## # (Optional) filter by genotype (e.g. Var == "VT"), if column exists:
## # apsim_train <- subset(apsim_train, Var == "VT")
##
## # 2. Tune RF hyperparameters using OOB RMSE
## tuning <- tune_rf_emulator(
##   train_df   = apsim_train,
##   response   = "yield",
##   mtry_grid  = seq(1, 9, by = 2),
##   ntree_grid = c(100, 200, 300, 500, 800, 1000),
##   seed       = 123
## )
## print(tuning$best)
##
## # 3. Fit final emulator
## best_mtry  <- tuning$best$mtry
## best_ntree <- tuning$best$ntree
##
## rf_model <- fit_rf_emulator(
##   train_df = apsim_train,
##   response = "yield",
##   mtry     = best_mtry,
##   ntree    = best_ntree,
##   seed     = 123
## )
##
## # 4. Project future yields under multiple GCMs
## gcm_list <- c("CanESM5", "CNRM_CM6_1", "CNRM_ESM2_1", "EC_Earth3", "MIROC6")
## project_gcm_scenarios(
##   model       = rf_model,
##   gcm_ids     = gcm_list,
##   input_dir   = "data/future_inputs",
##   output_dir  = "results/emulator_outputs",
##   input_prefix  = "future_",
##   output_prefix = "yield_projection_"
## )
