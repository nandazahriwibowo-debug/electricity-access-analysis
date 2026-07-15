# =====================================================================
# Spatial Analysis of Electricity Access Inequality in Indonesia
# Author: Nanda Zahri Wibowo
#
# Data: INDO-DAPOER (World Bank & BPS-Statistics Indonesia), 2019,
#       district level -- see data/README.md for full provenance.
#       Boundaries: geoBoundaries gbOpen IDN ADM2 (CC BY 4.0).
#
# Pipeline:
#   1. Load the assembled district dataset (built by build_dataset.R)
#   2. Exploratory analysis (distributions, correlations, rankings)
#   3. Spatial statistics: Moran's I, LISA, Getis-Ord Gi*, spatial lag
#   4. Predictive models: OLS, Random Forest, XGBoost, Elastic Net
#   5. Scenario-based policy simulation
#   6. Export everything the Rmd report needs to output/
# =====================================================================

# ---------------------------------------------------------------------
# SECTION 0 -- Locate the project folder (works regardless of where R
# was started from, so "Dataset not found" never occurs again).
# ---------------------------------------------------------------------
get_script_dir <- function() {
  # 1) Rscript on the command line
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  # 2) RStudio: source button / editor
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path,
                  error = function(e) "")
    if (nzchar(p)) return(dirname(normalizePath(p)))
  }
  # 3) source("...") from another script
  for (fr in rev(sys.frames())) {
    if (!is.null(fr$ofile)) return(dirname(normalizePath(fr$ofile)))
  }
  # 4) Fallback: current working directory
  getwd()
}

PROJECT_DIR <- get_script_dir()
setwd(PROJECT_DIR)
cat("Working directory set to:", PROJECT_DIR, "\n")

DATA_CSV   <- "data/dapoer_district_2019.csv"
OUTPUT_DIR <- "output"
CV_FOLDS   <- 10
set.seed(2019)

start_time <- Sys.time()
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ---------------------------------------------------------------------
# SECTION 1 -- Packages
# ---------------------------------------------------------------------
load_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

pkgs <- c("tidyverse", "sf", "spdep", "spatialreg", "caret",
          "ggcorrplot", "randomForest", "xgboost", "glmnet", "pdp")
invisible(lapply(pkgs, load_pkg))

cat("=====================================================\n")
cat(" Electricity access inequality, Indonesia (2019)\n")
cat(" District-level spatial + ML analysis\n")
cat("=====================================================\n\n")

# ---------------------------------------------------------------------
# SECTION 2 -- Data
# ---------------------------------------------------------------------
if (!file.exists(DATA_CSV)) {
  # Auto-rebuild the dataset from the raw INDO-DAPOER downloads
  # instead of stopping with an error.
  if (file.exists("build_dataset.R")) {
    cat("Dataset not found -- rebuilding it now via build_dataset.R ...\n")
    source("build_dataset.R")
  }
  if (!file.exists(DATA_CSV)) {
    stop("Dataset not found and could not be rebuilt. Make sure this ",
         "script sits in the project folder next to build_dataset.R, ",
         "data/, and output/. Current directory: ", getwd())
  }
}

data <- read_csv(DATA_CSV, show_col_types = FALSE) %>%
  mutate(
    # Outcome: share of households WITHOUT electricity
    access_deficit = 100 - electricity_access,
    # Monthly expenditure in thousand IDR for readable coefficients
    expenditure_pc_thousand = expenditure_pc / 1000,
    log_pop_density = log10(population_density),
    log_grdp_pc     = log10(grdp_per_capita),
    is_city         = as.integer(district_type == "city")
  ) %>%
  # 6 districts dropped: 3 created in 2014 lack population; 3 Papua districts lack water_access
  drop_na(access_deficit, hdi, poverty_rate, population_density,
          grdp_per_capita, water_access, literacy_rate)

cat("Districts analysed:", nrow(data), "\n")
cat("Households without electricity, national median:",
    round(median(data$access_deficit), 2), "%\n\n")

predictors <- c("hdi", "poverty_rate", "expenditure_pc_thousand",
                "log_pop_density", "log_grdp_pc", "water_access",
                "literacy_rate", "is_city")

# ---------------------------------------------------------------------
# SECTION 3 -- Exploratory analysis
# ---------------------------------------------------------------------
cat("[EDA] Summary of key variables\n")
print(summary(data %>% select(access_deficit, hdi, poverty_rate,
                              population_density, grdp_per_capita)))

p_dist <- data %>%
  select(access_deficit, hdi, poverty_rate, log_pop_density) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "steelblue", alpha = 0.7) +
  geom_density(color = "darkred", linewidth = 0.8) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distributions of key variables (508 districts, 2019)",
       x = NULL, y = "Density") +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, "distribution_plots.png"), p_dist,
       width = 10, height = 7, dpi = 150)

corr <- data %>%
  select(access_deficit, all_of(setdiff(predictors, "is_city"))) %>%
  cor(use = "complete.obs", method = "spearman")
write_csv(as.data.frame(corr) %>% rownames_to_column("variable"),
          file.path(OUTPUT_DIR, "correlation_matrix.csv"))

p_corr <- ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE,
                     lab_size = 2.8,
                     colors = c("#B2182B", "white", "#2166AC"),
                     title = "Spearman correlations", ggtheme = theme_minimal)
ggsave(file.path(OUTPUT_DIR, "correlation_matrix.png"), p_corr,
       width = 8, height = 7, dpi = 150)

prov_summary <- data %>%
  group_by(province) %>%
  summarise(mean_deficit = mean(access_deficit),
            worst_deficit = max(access_deficit),
            districts = n(), .groups = "drop") %>%
  arrange(desc(mean_deficit))
write_csv(prov_summary, file.path(OUTPUT_DIR, "province_summary.csv"))

p_prov <- prov_summary %>%
  ggplot(aes(reorder(province, mean_deficit), mean_deficit)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Households without electricity, by province (2019)",
       x = NULL, y = "Mean share of households without electricity (%)") +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, "disparity_by_province.png"), p_prov,
       width = 9, height = 8, dpi = 150)

# ---------------------------------------------------------------------
# SECTION 4 -- Spatial statistics
# ---------------------------------------------------------------------
cat("\n[SPATIAL] Building k-nearest-neighbour weights (k = 5)\n")
coords <- as.matrix(data[, c("longitude", "latitude")])
nb     <- knn2nb(knearneigh(coords, k = 5))
listw  <- nb2listw(nb, style = "W")

moran <- moran.test(data$access_deficit, listw)
cat("Moran's I:", round(moran$estimate[1], 4),
    " p-value:", format.pval(moran$p.value), "\n")
saveRDS(moran, file.path(OUTPUT_DIR, "moran_test.rds"))

# LISA
lisa <- localmoran(data$access_deficit, listw)
z    <- as.numeric(scale(data$access_deficit))
lagz <- lag.listw(listw, z)
data$lisa_cluster <- case_when(
  lisa[, "Pr(z != E(Ii))"] >= 0.05 ~ "Not significant",
  z >= 0 & lagz >= 0 ~ "High-High",
  z <  0 & lagz <  0 ~ "Low-Low",
  z >= 0 & lagz <  0 ~ "High-Low",
  TRUE               ~ "Low-High"
)

# Getis-Ord Gi*
gi <- localG(data$access_deficit, listw)
data$hotspot <- case_when(
  as.numeric(gi) >  1.96 ~ "Hotspot",
  as.numeric(gi) < -1.96 ~ "Coldspot",
  TRUE                   ~ "Not significant"
)
cat("\nLISA cluster counts:\n"); print(table(data$lisa_cluster))
cat("\nGi* hotspot counts:\n");  print(table(data$hotspot))

# Spatial lag regression
lag_model <- lagsarlm(
  access_deficit ~ hdi + poverty_rate + log_pop_density +
    log_grdp_pc + water_access + is_city,
  data = data, listw = listw)
lag_sum <- summary(lag_model)
capture.output(lag_sum, file = file.path(OUTPUT_DIR, "spatial_lag_model.txt"))
saveRDS(lag_model, file.path(OUTPUT_DIR, "spatial_lag_model.rds"))
cat("\nSpatial lag model: rho =", round(lag_model$rho, 3),
    " AIC =", round(AIC(lag_model), 1), "\n")

# Maps (point maps at district centroids)
map_theme <- theme_minimal() + theme(legend.position = "right")

p_map <- ggplot(data, aes(longitude, latitude, color = access_deficit)) +
  geom_point(size = 1.6, alpha = 0.85) +
  scale_color_viridis_c(option = "inferno", direction = -1) +
  labs(title = "Households without electricity, 2019",
       subtitle = "District centroids; darker = larger deficit",
       x = "Longitude", y = "Latitude", color = "Deficit (%)") +
  map_theme
ggsave(file.path(OUTPUT_DIR, "map_access_deficit.png"), p_map,
       width = 10, height = 5, dpi = 150)

p_lisa <- ggplot(data, aes(longitude, latitude, color = lisa_cluster)) +
  geom_point(size = 1.6, alpha = 0.85) +
  scale_color_manual(values = c("High-High" = "#D73027", "Low-Low" = "#4575B4",
                                "High-Low" = "#FC8D59", "Low-High" = "#91BFDB",
                                "Not significant" = "grey80")) +
  labs(title = "LISA clusters of electricity access deficit",
       x = "Longitude", y = "Latitude", color = "Cluster") +
  map_theme
ggsave(file.path(OUTPUT_DIR, "map_lisa_clusters.png"), p_lisa,
       width = 10, height = 5, dpi = 150)

p_hot <- ggplot(data, aes(longitude, latitude, color = hotspot)) +
  geom_point(size = 1.6, alpha = 0.85) +
  scale_color_manual(values = c("Hotspot" = "#D73027", "Coldspot" = "#4575B4",
                                "Not significant" = "grey80")) +
  labs(title = "Getis-Ord Gi* hotspots of electricity access deficit",
       x = "Longitude", y = "Latitude", color = "Category") +
  map_theme
ggsave(file.path(OUTPUT_DIR, "map_hotspots.png"), p_hot,
       width = 10, height = 5, dpi = 150)

# ---------------------------------------------------------------------
# SECTION 5 -- Predictive modelling
# ---------------------------------------------------------------------
cat("\n[ML] Training four models with", CV_FOLDS, "-fold CV\n")

model_df <- data %>% select(access_deficit, all_of(predictors))

idx   <- createDataPartition(model_df$access_deficit, p = 0.8, list = FALSE)
train <- model_df[idx, ]
test  <- model_df[-idx, ]

ctrl <- trainControl(method = "cv", number = CV_FOLDS)
form <- access_deficit ~ .

cache <- file.path(OUTPUT_DIR, "model_cache")
if (!dir.exists(cache)) dir.create(cache)

fit_cached <- function(name, expr) {
  path <- file.path(cache, paste0(name, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  m <- expr()
  saveRDS(m, path)
  m
}

lm_m  <- fit_cached("lm",  function() train(form, train, method = "lm",
                                            trControl = ctrl))
rf_m  <- fit_cached("rf",  function() train(form, train, method = "rf",
                                            trControl = ctrl,
                                            tuneGrid = expand.grid(mtry = c(2, 4, 6)),
                                            importance = TRUE, ntree = 500))
en_m  <- fit_cached("en",  function() train(form, train, method = "glmnet",
                                            trControl = ctrl,
                                            tuneGrid = expand.grid(
                                              alpha = seq(0, 1, 0.25),
                                              lambda = 10^seq(-3, 0, length.out = 8))))

# XGBoost fitted natively (caret's xgbTree wrapper is incompatible with
# xgboost >= 3.0); nrounds chosen by xgb.cv on the training set.
xgb_m <- fit_cached("xgb", function() {
  X <- as.matrix(train[, predictors])
  y <- train$access_deficit
  params <- list(objective = "reg:squarederror", max_depth = 4,
                 eta = 0.05, subsample = 0.8, colsample_bytree = 0.8)
  cv <- xgb.cv(params = params, data = xgb.DMatrix(X, label = y),
               nrounds = 600, nfold = CV_FOLDS,
               early_stopping_rounds = 30, verbose = 0)
  xgb.train(params = params, data = xgb.DMatrix(X, label = y),
            nrounds = cv$early_stop$best_iteration)
})

predict_any <- function(model, newdata) {
  if (inherits(model, "xgb.Booster")) {
    predict(model, xgb.DMatrix(as.matrix(newdata[, predictors])))
  } else {
    predict(model, newdata)
  }
}

evaluate <- function(model, newdata) {
  p <- predict_any(model, newdata)
  c(RMSE = sqrt(mean((newdata$access_deficit - p)^2)),
    MAE  = mean(abs(newdata$access_deficit - p)),
    R2   = cor(newdata$access_deficit, p)^2)
}

perf <- bind_rows(
  Linear         = evaluate(lm_m,  test),
  `Random Forest`= evaluate(rf_m,  test),
  XGBoost        = evaluate(xgb_m, test),
  `Elastic Net`  = evaluate(en_m,  test),
  .id = "Model") %>%
  arrange(RMSE)
cat("\nHold-out performance:\n"); print(as.data.frame(perf))
write_csv(perf, file.path(OUTPUT_DIR, "model_performance.csv"))

# Variable importance (best tree model)
best_name <- perf$Model[1]
best_tree <- switch(best_name, "XGBoost" = xgb_m, "Random Forest" = rf_m, rf_m)

if (inherits(best_tree, "xgb.Booster")) {
  imp <- xgb.importance(model = best_tree) %>%
    as_tibble() %>%
    transmute(variable = Feature, Overall = 100 * Gain / max(Gain))
} else {
  imp <- varImp(best_tree)$importance %>%
    rownames_to_column("variable") %>%
    as_tibble()
}
imp <- imp %>% arrange(desc(Overall))
write_csv(imp, file.path(OUTPUT_DIR, "variable_importance.csv"))

p_imp <- imp %>%
  ggplot(aes(reorder(variable, Overall), Overall)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = paste("Variable importance,", best_name),
       x = NULL, y = "Relative importance (0-100)") +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, "feature_importance.png"), p_imp,
       width = 8, height = 5, dpi = 150)

# Partial dependence for the top-3 predictors
top3 <- head(imp$variable, 3)
pdp_list <- map(top3, function(v) {
  if (inherits(best_tree, "xgb.Booster")) {
    partial(best_tree, pred.var = v, train = as.matrix(train[, predictors])) %>%
      rename(x = 1) %>% mutate(variable = v)
  } else {
    partial(best_tree, pred.var = v, train = train) %>%
      rename(x = 1) %>% mutate(variable = v)
  }
})
pdp_df <- bind_rows(pdp_list)
p_pdp <- ggplot(pdp_df, aes(x, yhat)) +
  geom_line(color = "steelblue", linewidth = 1) +
  facet_wrap(~variable, scales = "free_x") +
  labs(title = "Partial dependence, top-3 predictors",
       x = "Predictor value", y = "Predicted deficit (%)") +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, "partial_dependence.png"), p_pdp,
       width = 10, height = 4, dpi = 150)

# ---------------------------------------------------------------------
# SECTION 6 -- Policy simulation
# ---------------------------------------------------------------------
cat("\n[SIM] Scenario analysis using the best model (", best_name, ")\n")

simulate <- function(label, mutate_fn) {
  newdata <- mutate_fn(model_df)
  tibble(Scenario = label,
         mean_deficit = mean(predict_any(best_tree, newdata)))
}

scenarios <- bind_rows(
  simulate("Baseline",              identity),
  simulate("HDI +5 points",         function(d) mutate(d, hdi = pmin(hdi + 5, 100))),
  simulate("Poverty -25%",          function(d) mutate(d, poverty_rate = poverty_rate * 0.75)),
  simulate("Water access +10 pp",   function(d) mutate(d, water_access = pmin(water_access + 10, 100))),
  simulate("Combined",              function(d) mutate(d,
                                      hdi = pmin(hdi + 5, 100),
                                      poverty_rate = poverty_rate * 0.75,
                                      water_access = pmin(water_access + 10, 100)))
) %>%
  mutate(change_vs_baseline = mean_deficit - mean_deficit[Scenario == "Baseline"])

print(as.data.frame(scenarios))
write_csv(scenarios, file.path(OUTPUT_DIR, "policy_simulation.csv"))

p_sim <- scenarios %>%
  ggplot(aes(reorder(Scenario, -mean_deficit), mean_deficit)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.2f", mean_deficit)), vjust = -0.4, size = 3.4) +
  labs(title = "Predicted mean access deficit under policy scenarios",
       x = NULL, y = "Mean predicted deficit (%)") +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, "policy_simulation.png"), p_sim,
       width = 8, height = 5, dpi = 150)

# ---------------------------------------------------------------------
# SECTION 7 -- Export final table for the Rmd
# ---------------------------------------------------------------------
write_csv(data, file.path(OUTPUT_DIR, "final_analysis_results.csv"))

cat("\n=====================================================\n")
cat(" Done in", round(difftime(Sys.time(), start_time, units = "mins"), 2),
    "minutes. Outputs in:", normalizePath(OUTPUT_DIR), "\n")
cat("=====================================================\n")
