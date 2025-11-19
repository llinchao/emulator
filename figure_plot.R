############################################################
# Example script for visualizing RF emulator performance
# - Hyperparameter tuning (R2 / RMSE vs mtry)
# - APSIM vs KGML scatter plots with R2 & RMSE
# - Variable importance bar plots
# - Partial dependence plots (PDPs) for VS and VT
#
# This script assumes a project structure like:
#   data/
#     Emulator_data/
#       paramater_VS.csv
#       paramater_VT.csv
#       Performance_VS.csv
#       Performance_VT.csv
#       Imp_VS.csv
#       Imp_VT.csv
#       Train_1.csv
#     PDP/
#       Variable_type.csv
#       PDP_VS/<Variable>.csv
#       PDP_VT/<Variable>.csv
#   figures/
#
# You can adapt file names and paths as needed.
############################################################

# -------------------------
# 0. Packages and paths
# -------------------------
library(ggplot2)
library(ggpubr)
library(dplyr)
library(readr)
library(Metrics)
library(ggsci)

# Use relative paths so the script works as a general example
data_dir   <- "data/Emulator_data"
pdp_dir    <- "data/PDP"
fig_dir    <- "figures"

# Create figures directory if it does not exist
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

############################################################
# 1. RF hyperparameter tuning: R2 & RMSE vs mtry
############################################################

# Helper to read tuning results and add a label for the variable set
read_tuning_results <- function(var_name, data_dir) {
  file_path <- file.path(data_dir, paste0("paramater_", var_name, ".csv"))
  df <- read.csv(file_path)
  df$Var <- var_name
  df
}

# Read two example variable sets: VS and VT
tuning_vs <- read_tuning_results("VS", data_dir)
tuning_vt <- read_tuning_results("VT", data_dir)
tuning_all <- rbind(tuning_vs, tuning_vt)

# Function to generate a tuning plot for a given metric
plot_tuning_metric <- function(data, metric_col, y_label) {
  ggplot(data, aes(x = mtry, y = .data[[metric_col]],
                   group = ntree, color = as.factor(ntree))) +
    geom_line(alpha = 0.75) +
    geom_point(size = 2) +
    labs(x = "mtry",
         y = y_label,
         color = "ntree") +
    scale_color_brewer(palette = "Set1") +
    facet_wrap(~ Var, scales = "free_y") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "transparent", colour = "black"),
      plot.background  = element_rect(fill = "white", colour = "white"),
      panel.grid.major = element_line(color = "lightgrey", size = 0.5),
      panel.grid.minor = element_line(color = "lightgrey", size = 0.25),
      legend.key       = element_rect(fill = "white", colour = NA),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.position  = "bottom",
      strip.text       = element_text(size = 10, color = "black")
    ) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE))
}

plot_r2   <- plot_tuning_metric(tuning_all, "R2",   expression(R^2))
plot_rmse <- plot_tuning_metric(tuning_all, "RMSE", "RMSE")

tuning_combined <- ggarrange(plot_r2, plot_rmse, ncol = 1, nrow = 2)

ggsave(
  filename = file.path(fig_dir, "tuning_R2_RMSE_vs_mtry.png"),
  plot     = tuning_combined,
  width    = 20,
  height   = 20,
  units    = "cm",
  dpi      = 300
)

############################################################
# 2. APSIM vs KGML scatter plots with R2 and RMSE
############################################################

# Helper to compute R2 and RMSE by Set and return annotation data
compute_performance_metrics <- function(df) {
  df %>%
    group_by(Set) %>%
    summarise(
      R2   = cor(APSIM_predicted, KGML_predicted)^2,
      RMSE = rmse(APSIM_predicted, KGML_predicted),
      .groups = "drop"
    ) %>%
    mutate(
      text = paste0("R2 = ", round(R2, 2),
                    "  RMSE = ", round(RMSE, 2), " kg/ha"),
      x = -Inf,
      y = Inf,
      hjust = -0.1,
      vjust = c(1.9, 3.6)  # Offset lines slightly so they don't overlap
    )
}

# Helper to make a scatter plot given a performance file
plot_performance <- function(file_name, title) {
  df <- read.csv(file.path(data_dir, file_name))
  df$Set <- factor(df$Set, levels = c("Validation", "Calibration"))
  
  metrics <- compute_performance_metrics(df)
  
  ggplot() +
    # Validation points
    geom_point(
      data = df[df$Set == "Validation", ],
      aes(x = APSIM_predicted, y = KGML_predicted, color = Set),
      size  = 1.2,
      alpha = 0.55
    ) +
    # Calibration points
    geom_point(
      data = df[df$Set == "Calibration", ],
      aes(x = APSIM_predicted, y = KGML_predicted, color = Set),
      size  = 1.2,
      alpha = 0.25
    ) +
    # Regression lines by Set
    geom_smooth(
      data = df,
      aes(x = APSIM_predicted, y = KGML_predicted, color = Set),
      method = "lm",
      se = FALSE,
      show.legend = FALSE
    ) +
    # Text annotations with R2 and RMSE
    geom_text(
      data = metrics,
      aes(x = x, y = y, label = text, color = Set,
          hjust = hjust, vjust = vjust),
      size = 4.5,
      show.legend = FALSE
    ) +
    scale_color_manual(values = c(
      "Calibration" = "violetred",
      "Validation"  = "dodgerblue1"
    )) +
    labs(
      x     = "APSIM prediction (kg/ha)",
      y     = "KGML prediction (kg/ha)",
      title = title
    ) +
    theme_bw() +
    theme(legend.position = "top") +
    guides(color = guide_legend(title = NULL))
}

perf_vs_plot <- plot_performance("Performance_VS.csv", "VS")
perf_vt_plot <- plot_performance("Performance_VT.csv", "VT")

perf_combined <- ggarrange(
  perf_vs_plot, perf_vt_plot,
  ncol = 2, nrow = 1,
  common.legend = TRUE,
  legend = "bottom"
)

ggsave(
  filename = file.path(fig_dir, "performance_APSIM_vs_KGML.png"),
  plot     = perf_combined,
  width    = 26,
  height   = 13,
  units    = "cm",
  dpi      = 300
)

############################################################
# 3. Variable importance bar plots (percent contribution)
############################################################

# Helper to read importance and compute percentage within each set
read_importance <- function(file_name, group_label) {
  # Example: first row may be a header/metadata row to skip
  df <- read_csv(
    file.path(data_dir, file_name),
    col_names = c("Variable", "Importance"),
    skip = 1
  )
  df$Group <- group_label
  
  df %>%
    mutate(
      Importance = as.numeric(Importance),
      Percent    = Importance / sum(Importance) * 100,
      Var_plot   = reorder(Variable, Percent)
    )
}

imp_vs <- read_importance("Imp_VS.csv", "VS")
imp_vt <- read_importance("Imp_VT.csv", "VT")

# Define a palette with enough distinct colors
colors_31 <- c(
  pal_npg("nrc")(10),
  pal_jco("default")(10),
  pal_d3("category20")(20)
)[4:34]

plot_importance <- function(df, title) {
  ggplot(df, aes(x = Var_plot, y = Percent, fill = Variable)) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = colors_31) +
    labs(x = NULL, y = "Importance (%)", title = title) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y      = element_text(size = 9),
      legend.position  = "none",
      plot.title       = element_text(hjust = 0.5),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA)
    )
}

imp_vs_plot <- plot_importance(imp_vs, "VS")
imp_vt_plot <- plot_importance(imp_vt, "VT")

imp_combined <- ggarrange(imp_vs_plot, imp_vt_plot, ncol = 2, align = "hv")

ggsave(
  filename = file.path(fig_dir, "importance_VS_VT_pct.png"),
  plot     = imp_combined,
  width    = 32,
  height   = 18,
  units    = "cm",
  dpi      = 300
)

############################################################
# 4. Partial dependence plots (PDPs) for VS and VT
############################################################

# Variable grouping metadata:
#   Variable_type.csv should have columns: Variable, Type, Unit
var_meta <- read.csv(file.path(pdp_dir, "Variable_type.csv"))

# Group names and layout settings for each group
type_names <- c("Basic", "ETC", "EPC", "Soil")
n_rows     <- c(2, 2, 2, 3)   # number of rows in PDP panel by group
n_cols     <- c(3, 4, 4, 3)   # number of columns in PDP panel by group
fig_width  <- c(15, 15, 15, 20)
fig_height <- c(30, 40, 40, 30)

# Training data used to define quantile ranges for shading
train_data <- read.csv(file.path(data_dir, "Train_1.csv"))
train_cols <- colnames(train_data)

# Function to build PDP for a single variable (VS and VT together)
build_pdp_for_variable <- function(var_name, var_unit, train_df) {
  col_idx <- which(train_cols == var_name)
  predictor_vals <- train_df[, col_idx]
  
  # Use extreme quantiles to trim PDP curves
  q_low_all  <- quantile(predictor_vals, probs = 0.001, na.rm = TRUE)
  q_high_all <- quantile(predictor_vals, probs = 0.999, na.rm = TRUE)
  
  # Load PDP for VT
  pdp_vt_file <- file.path(pdp_dir, "PDP_VT", paste0(var_name, ".csv"))
  pdp_vt <- read.csv(pdp_vt_file)
  pdp_vt <- pdp_vt[pdp_vt[, 1] >= q_low_all & pdp_vt[, 1] <= q_high_all, ]
  pdp_vt$var  <- "VT"
  pdp_vt$yhat <- (pdp_vt$yhat - mean(pdp_vt$yhat)) / mean(pdp_vt$yhat) * 100
  
  # Load PDP for VS
  pdp_vs_file <- file.path(pdp_dir, "PDP_VS", paste0(var_name, ".csv"))
  pdp_vs <- read.csv(pdp_vs_file)
  pdp_vs <- pdp_vs[pdp_vs[, 1] >= q_low_all & pdp_vs[, 1] <= q_high_all, ]
  pdp_vs$var  <- "VS"
  pdp_vs$yhat <- (pdp_vs$yhat - mean(pdp_vs$yhat)) / mean(pdp_vs$yhat) * 100
  
  pdp_all <- rbind(pdp_vt, pdp_vs)
  colnames(pdp_all) <- c("x", "yield", "var")
  
  # Central 10每90% range for shading
  q_low  <- quantile(predictor_vals, probs = 0.10, na.rm = TRUE)
  q_high <- quantile(predictor_vals, probs = 0.90, na.rm = TRUE)
  
  x_lab <- paste(var_name, var_unit)
  
  ggplot(pdp_all, aes(x = x, y = yield, color = var)) +
    annotate(
      "rect",
      xmin = q_low, xmax = q_high,
      ymin = -Inf, ymax = Inf,
      fill = "gray80", alpha = 0.5
    ) +
    geom_smooth(
      method = "loess",
      se = FALSE,
      alpha = 0.2,
      span = 0.4,
      aes(fill = NULL)
    ) +
    scale_color_manual(values = c(VS = "#008B8B", VT = "#FFCC22")) +
    labs(
      x = x_lab,
      y = "Yield change (%)"
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position      = "top",
      legend.title         = element_blank(),
      legend.text          = element_text(size = 13),
      legend.key.size      = unit(1.2, "lines"),
      panel.background     = element_rect(fill = "white", color = NA),
      plot.background      = element_rect(fill = "white", color = NA),
      legend.background    = element_rect(fill = "white", color = NA),
      legend.box.background = element_rect(fill = "white", color = NA),
      panel.grid           = element_line(color = "gray90")
    )
}

# Loop over groups and save one PDP panel per group
for (i in seq_along(type_names)) {
  group_type <- type_names[i]
  
  idx_group <- which(var_meta$Type == group_type)
  if (length(idx_group) == 0) next
  
  plot_list <- vector("list", length(idx_group))
  
  for (j in seq_along(idx_group)) {
    v_name <- var_meta$Variable[idx_group[j]]
    v_unit <- var_meta$Unit[idx_group[j]]
    
    plot_list[[j]] <- build_pdp_for_variable(
      var_name = v_name,
      var_unit = v_unit,
      train_df = train_data
    )
  }
  
  pdp_panel <- ggarrange(
    plotlist      = plot_list,
    ncol          = n_cols[i],
    nrow          = n_rows[i],
    common.legend = TRUE,
    legend        = "bottom"
  )
  
  out_file <- file.path(fig_dir, paste0("PDP_", group_type, ".png"))
  
  ggsave(
    filename = out_file,
    plot     = pdp_panel,
    width    = fig_height[i],
    height   = fig_width[i],
    units    = "cm",
    dpi      = 300
  )
}

###############################################
## 0. Load packages (once)
###############################################

library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(reshape2)
library(raster)
library(sf)
library(RColorBrewer)
###############################################
## 5. Percentile configuration + helper tools
## (central place to control all clipping rules)
###############################################

## NOTE: Script assumes you have:
##   library(dplyr)
##   library(ggplot2)
##   library(sf)
##   library(raster)
## loaded somewhere in your workflow.

perc <- list(
  # Symmetric clipping of map values (e.g. -lim ... +lim)
  map_clip_sym_T = 0.05,   # temperature-type variables
  map_clip_sym_P = 0.05,   # precipitation-type variables
  
  # Histogram x-axis clipping (upper tail only)
  hist_clip_upper = 0.99,  # keep 99% of the data
  
  # Simple ※CI§ range for regional plots
  box_ci = c(0.1, 0.9),    # 10%每90% interval
  
  # ※Boxplot-like§ summaries (whisker, box, median, whisker)
  box_whisker = c(0.1, 0.25, 0.5, 0.75, 0.9)  # 10,25,50,75,90%
)

# You can adjust all clipping behaviour here in one place.
# Example for stronger tail clipping:
# perc$map_clip_sym_T  <- 0.001
# perc$map_clip_sym_P  <- 0.001
# perc$hist_clip_upper <- 0.999


## -------- basic helpers --------

# Internal check for percentile vectors
.check_probs <- function(probs, nm = "probs") {
  if (any(probs < 0 | probs > 1, na.rm = TRUE)) {
    stop(sprintf("%s must be between 0 and 1.", nm), call. = FALSE)
  }
  invisible(probs)
}

# Wrapper around quantile with NA handling and basic checks
safe_quantile <- function(x, probs, ...) {
  .check_probs(probs, "probs")
  stats::quantile(x, probs = probs, na.rm = TRUE, ...)
}

# Symmetric clip limit for maps (returns a single positive limit)
get_sym_clip <- function(x, p) {
  q <- safe_quantile(x, probs = c(p, 1 - p))
  max(abs(q))
}

# Apply symmetric clipping to a numeric vector (for map values)
clip_map_sym <- function(x, p) {
  lim <- get_sym_clip(x, p)
  x[x >  lim] <-  lim
  x[x < -lim] <- -lim
  x
}

# Clip values for histograms (lower/upper by quantiles)
clip_for_hist <- function(x,
                          upper_prob = perc$hist_clip_upper,
                          lower_prob = 0) {
  .check_probs(c(lower_prob, upper_prob), "lower/upper_prob")
  if (lower_prob > upper_prob) {
    stop("lower_prob must be <= upper_prob.", call. = FALSE)
  }
  qs <- safe_quantile(x, probs = c(lower_prob, upper_prob))
  x <- pmin(pmax(x, qs[1]), qs[2])
  x
}

# Boxplot-style summary from quantiles:
#   ymin  = lower whisker
#   lower = 25% (or user-provided)
#   middle= 50%
#   upper = 75%
#   ymax  = upper whisker
box_stats_from_quantile <- function(x,
                                    probs = perc$box_whisker) {
  if (length(probs) != 5L) {
    stop("probs for box_stats_from_quantile must have length 5.", call. = FALSE)
  }
  qs <- safe_quantile(x, probs = probs)
  data.frame(
    ymin   = qs[1],
    lower  = qs[2],
    middle = qs[3],
    upper  = qs[4],
    ymax   = qs[5]
  )
}

# Wrapper for ggplot::stat_summary(fun.data = ...)
box_stats_fun <- function(y) {
  box_stats_from_quantile(y, perc$box_whisker)
}

# Simple ※CI§ from quantiles for region barplots
ci_from_quantile <- function(x,
                             probs = perc$box_ci) {
  if (length(probs) != 2L) {
    stop("probs for ci_from_quantile must have length 2.", call. = FALSE)
  }
  qs <- safe_quantile(x, probs = probs)
  c(ymin = qs[1], ymax = qs[2])
}

###############################################
## 6. EXE_events1  (example structure)
##    每 maps + histograms using helper tools
###############################################

## (1) read + prepare data for EXE_events1
## NOTE: change file paths and object names to your own

# Example: stack of rasters per GCM
# files_exe1 <- list.files(
#   "path_to_EXE_events1_rasters",
#   pattern    = "EXE_events1_.*\\.tif$",
#   full.names = TRUE
# )
# rs_list_exe1 <- lapply(files_exe1, raster::stack)

# Example shapefile
# shp_world <- sf::st_read("path_to_shapefile/world.shp")

## (2) map plotting with symmetric clipping (temperature-type)
## Reuse this pattern for each raster stack / scenario.

plot_exe1_map <- function(r_exe1,
                          shp,
                          title_label = "EXE_events1") {
  
  # Convert raster to data frame (x, y, value)
  df <- as.data.frame(r_exe1, xy = TRUE)
  names(df)[3] <- "value"
  
  # Symmetric clipping using centralized percentile for T-type variables
  df$value_clip <- clip_map_sym(df$value, perc$map_clip_sym_T)
  
  ggplot() +
    geom_raster(
      data = df,
      aes(x = x, y = y, fill = value_clip)
    ) +
    geom_sf(
      data  = shp,
      fill  = NA,
      color = "grey40",
      size  = 0.2
    ) +
    scale_fill_distiller(palette = "RdBu", direction = -1) +
    coord_equal() +
    theme_bw() +
    labs(
      title = title_label,
      fill  = "Change"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_blank(),
      axis.title = element_blank()
    )
}

## (3) histogram of EXE_events1 changes

plot_exe1_hist <- function(df_long,
                           title_label = "EXE_events1 histogram") {
  
  # df_long: columns ※Value§ and e.g. ※GCM / Scenario / Var§
  df_long <- df_long %>%
    mutate(
      Value_clip = clip_for_hist(
        Value,
        upper_prob = perc$hist_clip_upper
      )
    )
  
  ggplot(df_long, aes(x = Value_clip)) +
    geom_histogram(
      aes(y = ..density..),
      bins  = 50,
      color = "black"
    ) +
    theme_bw() +
    labs(
      x     = "Change",
      y     = "Density",
      title = title_label
    )
}

###############################################
## 7. EXE_events2  每 similar logic (P-type)
###############################################

## (1) map plotting: precipitation-type variable

plot_exe2_map <- function(r_exe2,
                          shp,
                          title_label = "EXE_events2") {
  
  df <- as.data.frame(r_exe2, xy = TRUE)
  names(df)[3] <- "value"
  
  # Symmetric clipping, but use P-type percentile
  df$value_clip <- clip_map_sym(df$value, perc$map_clip_sym_P)
  
  ggplot() +
    geom_raster(
      data = df,
      aes(x = x, y = y, fill = value_clip)
    ) +
    geom_sf(
      data  = shp,
      fill  = NA,
      color = "grey40",
      size  = 0.2
    ) +
    scale_fill_distiller(palette = "BrBG", direction = -1) +
    coord_equal() +
    theme_bw() +
    labs(
      title = title_label,
      fill  = "Change"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_blank(),
      axis.title = element_blank()
    )
}

## (2) histogram as before

plot_exe2_hist <- function(df_long,
                           title_label = "EXE_events2 histogram") {
  
  df_long <- df_long %>%
    mutate(
      Value_clip = clip_for_hist(
        Value,
        upper_prob = perc$hist_clip_upper
      )
    )
  
  ggplot(df_long, aes(x = Value_clip)) +
    geom_histogram(
      aes(y = ..density..),
      bins  = 50,
      color = "black"
    ) +
    theme_bw() +
    labs(
      x     = "Change",
      y     = "Density",
      title = title_label
    )
}

###############################################
## 8. ECE_T2_T  每 histogram only (temperature)
###############################################

plot_ECE_T2_T_hist <- function(df_long,
                               title_label = "ECE T2 每 T histogram") {
  
  df_long <- df_long %>%
    mutate(
      Value_clip = clip_for_hist(
        Value,
        upper_prob = perc$hist_clip_upper
      )
    )
  
  ggplot(df_long, aes(x = Value_clip)) +
    geom_histogram(
      aes(y = ..density..),
      bins  = 50,
      color = "black"
    ) +
    theme_bw() +
    labs(
      x     = "忖T (～C)",
      y     = "Density",
      title = title_label
    )
}

###############################################
## 9. ECE_T2_P 每 map (P-type) + histogram
###############################################

plot_ECE_T2_P_map <- function(r_ECE,
                              shp,
                              title_label = "ECE T2 每 P") {
  
  df <- as.data.frame(r_ECE, xy = TRUE)
  names(df)[3] <- "value"
  
  df$value_clip <- clip_map_sym(df$value, perc$map_clip_sym_P)
  
  ggplot() +
    geom_raster(
      data = df,
      aes(x = x, y = y, fill = value_clip)
    ) +
    geom_sf(
      data  = shp,
      fill  = NA,
      color = "grey40",
      size  = 0.2
    ) +
    scale_fill_distiller(palette = "BrBG", direction = -1) +
    coord_equal() +
    theme_bw() +
    labs(
      title = title_label,
      fill  = "忖P"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid = element_blank(),
      axis.title = element_blank()
    )
}

plot_ECE_T2_P_hist <- function(df_long,
                               title_label = "ECE T2 每 P histogram") {
  
  df_long <- df_long %>%
    mutate(
      Value_clip = clip_for_hist(
        Value,
        upper_prob = perc$hist_clip_upper
      )
    )
  
  ggplot(df_long, aes(x = Value_clip)) +
    geom_histogram(
      aes(y = ..density..),
      bins  = 50,
      color = "black"
    ) +
    theme_bw() +
    labs(
      x     = "忖P",
      y     = "Density",
      title = title_label
    )
}

###############################################
## 10. Crop yield change distributions
##     每 boxplot style using box_stats_fun()
###############################################

## df_yield_change: columns
##   Region (factor), Scenario, GCM, DeltaYield (change in % or t/ha)

plot_yield_change_box <- function(df_yield_change,
                                  title_label = "Yield change distributions") {
  
  ggplot(df_yield_change,
         aes(x = Region, y = DeltaYield)) +
    stat_summary(
      fun.data = box_stats_fun,
      geom     = "boxplot",
      fill     = "white"
    ) +
    theme_bw() +
    labs(
      x     = "Region",
      y     = "Yield change",
      title = title_label
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

###############################################
## 11. Regional mean + ※CI§ barplots
##     (for yield gap, production benefit, etc.)
###############################################

## df_reg: columns
##   Region, Scenario, Value (e.g., yield_gap_change)

summarise_region_ci <- function(df_reg) {
  df_reg %>%
    group_by(Region, Scenario) %>%
    summarise(
      mean_val = mean(Value, na.rm = TRUE),
      ymin     = ci_from_quantile(Value, perc$box_ci)["ymin"],
      ymax     = ci_from_quantile(Value, perc$box_ci)["ymax"],
      .groups  = "drop"
    )
}

plot_region_bar_ci <- function(summary_reg,
                               title_label = "Regional mean change") {
  
  ggplot(summary_reg,
         aes(x = Region, y = mean_val, fill = Scenario)) +
    geom_col(position = position_dodge(width = 0.7)) +
    geom_errorbar(
      aes(ymin = ymin, ymax = ymax),
      width     = 0.2,
      position  = position_dodge(width = 0.7)
    ) +
    theme_bw() +
    labs(
      x     = "Region",
      y     = "Change",
      title = title_label,
      fill  = "Scenario"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

###############################################
## 12. Harvested area / production benefit
##     每 same logic as section 11 but other variables
###############################################

## df_area: Region, Scenario, AreaChange
## df_prod: Region, Scenario, ProdChange

plot_area_benefit <- function(df_area,
                              title_label = "Harvested area change") {
  
  sum_area <- df_area %>%
    group_by(Region, Scenario) %>%
    summarise(
      mean_val = mean(AreaChange, na.rm = TRUE),
      ymin     = ci_from_quantile(AreaChange, perc$box_ci)["ymin"],
      ymax     = ci_from_quantile(AreaChange, perc$box_ci)["ymax"],
      .groups  = "drop"
    )
  
  plot_region_bar_ci(sum_area, title_label = title_label) +
    ylab("Harvested area change")
}

plot_prod_benefit <- function(df_prod,
                              title_label = "Production benefit") {
  
  sum_prod <- df_prod %>%
    group_by(Region, Scenario) %>%
    summarise(
      mean_val = mean(ProdChange, na.rm = TRUE),
      ymin     = ci_from_quantile(ProdChange, perc$box_ci)["ymin"],
      ymax     = ci_from_quantile(ProdChange, perc$box_ci)["ymax"],
      .groups  = "drop"
    )
  
  plot_region_bar_ci(sum_prod, title_label = title_label) +
    ylab("Production change")
}
