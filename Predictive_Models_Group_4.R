#### ---- Setup ---- ####

# install.packages(c(
#   "mlr3verse", "readxl", "data.table", "mlr3learners",
#   "mlr3pipelines", "ranger", "ggplot2", "paradox", "future", "mlr3viz",
#   "glmnet"
# ))

library(readxl)
library(data.table)
library(mlr3)
library(mlr3tuning)
library(mlr3learners)
library(mlr3pipelines)
library(ggplot2)
library(paradox)
library(future)
library(mlr3viz)
library(glmnet)

# working directory
setwd("C:/Users/leima/OneDrive/Documents/_HSG/Semester_1/Machine Learning in R")


#### ---- Data loading & cleaning ---- ####

df <- as.data.table(read_excel("Final_Dataset_after_peerreview.xlsx"))

# remove UUID and keep only Lalique due to data availability
df[, UUID := NULL]
df <- df[Sort == "Lalique"]

# remove Weight outliers
Q1  <- quantile(df$Weight, 0.25, na.rm = TRUE)
Q3  <- quantile(df$Weight, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
df <- df[Weight >= lower_bound & Weight <= upper_bound]

df_copy <- copy(df)

# clean column names
names(df) <- gsub("%", "_pct", names(df))
names(df) <- gsub("°", "_deg", names(df))
names(df) <- gsub(" ", "_",   names(df))

# factorize character columns
char_cols <- names(df)[sapply(df, is.character)]
if (length(char_cols) > 0) {
  df[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]
}


#### ---- Define Task and Train/Test Split ---- ####

task_cabbage <- TaskRegr$new(
  id      = "cabbage_weight",
  backend = df,
  target  = "Weight"
)

set.seed(123)
splits <- partition(task_cabbage, ratio = 0.8)


#### ---- Dummy Model ---- ####

# predict the global median weight for all observations
global_median <- median(df$Weight, na.rm = TRUE)

# create prediction
dummy_pred <- data.table(
  truth    = df[splits$test, Weight],
  response = rep(global_median, length(splits$test))
)

# evaluation
dummy_rmse <- sqrt(mean((dummy_pred$response - dummy_pred$truth)^2))
dummy_mae  <- mean(abs(dummy_pred$response - dummy_pred$truth))

cat("Dummy Model RMSE:", round(dummy_rmse, 3), "\n")
cat("Dummy Model MAE:",  round(dummy_mae, 3),  "\n")

# plot
ggplot(dummy_pred, aes(x = truth, y = response)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(
    x     = "Actual Weight",
    y     = "Predicted Weight",
    title = "Predicted vs Actual - Dummy Model"
  )


#### ---- Linear Regression Model ---- ####

# pipeline: remove constant features -> impute -> linear regression
lm_piped <- as_learner(
  po("removeconstants") %>>%
    po("imputehist") %>>%
    lrn("regr.lm")
)

# train and predict
lm_piped$train(task_cabbage, splits$train)
pred_lm <- lm_piped$predict(task_cabbage, splits$test)

# results
measures_lm <- msrs(c("regr.rmse", "regr.mae", "regr.rsq"))
pred_lm$score(measures_lm)

mae_lm_single  <- pred_lm$score(msr("regr.mae"))
rmse_lm_single <- pred_lm$score(msr("regr.rmse"))
rsq_lm_single  <- pred_lm$score(msr("regr.rsq"))

cat("Linear Regression MAE:",  round(mae_lm_single, 3), "\n")
cat("Linear Regression RMSE:", round(rmse_lm_single, 3), "\n")
cat("Linear Regression R²:",   round(rsq_lm_single, 3), "\n")

# plot
pdt_lm <- as.data.table(pred_lm)
ggplot(pdt_lm, aes(x = truth, y = response)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(
    x     = "Actual Weight",
    y     = "Predicted Weight",
    title = "Predicted vs Actual - Linear Regression"
  )

# results
summary(lm_piped$model$`regr.lm`$model)


#### ---- Elastic Net Model ---- ####

enet_learner <- if ("regr.cv_glmnet" %in% mlr_learners$keys()) {
  lrn("regr.cv_glmnet", alpha = 0.5)
} else {
  lrn("regr.glmnet", alpha = 0.5)
}

enet_piped <- as_learner(
  po("removeconstants") %>>%
    po("imputehist") %>>%
    po("encode") %>>%   # <<< factor -> dummy variables for glmnet
  enet_learner
)

# train and predict
enet_piped$train(task_cabbage, splits$train)
pred_enet <- enet_piped$predict(task_cabbage, splits$test)

# results
measures_enet <- msrs(c("regr.rmse", "regr.mae", "regr.rsq"))
pred_enet$score(measures_enet)

mae_enet_single  <- pred_enet$score(msr("regr.mae"))
rmse_enet_single <- pred_enet$score(msr("regr.rmse"))
rsq_enet_single  <- pred_enet$score(msr("regr.rsq"))

cat("Elastic Net MAE:",  round(mae_enet_single, 3), "\n")
cat("Elastic Net RMSE:", round(rmse_enet_single, 3), "\n")
cat("Elastic Net R²:",   round(rsq_enet_single, 3), "\n")

# plot
pdt_enet <- as.data.table(pred_enet)
ggplot(pdt_enet, aes(x = truth, y = response)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(
    x     = "Actual Weight",
    y     = "Predicted Weight",
    title = "Predicted vs Actual - Elastic Net"
  )


#### ---- Random Forest ---- ####

rf <- lrn(
  "regr.ranger",
  num.trees  = 500,
  importance = "permutation",
  seed       = 123
)

rf_piped <- as_learner(po("imputehist") %>>% rf)

# train and predict
rf_piped$train(task_cabbage, splits$train)
pred_rf <- rf_piped$predict(task_cabbage, splits$test)

# results
measures_rf <- msrs(c("regr.rmse", "regr.mae", "regr.rsq"))
pred_rf$score(measures_rf)

mae_rf_single  <- pred_rf$score(msr("regr.mae"))
rmse_rf_single <- pred_rf$score(msr("regr.rmse"))
rsq_rf_single  <- pred_rf$score(msr("regr.rsq"))

cat("Baseline RF MAE:", round(mae_rf_single, 3), "\n")

#plot
pdt_rf <- as.data.table(pred_rf)
ggplot(pdt_rf, aes(x = truth, y = response)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(
    x     = "Actual Weight",
    y     = "Predicted Weight",
    title = "Predicted vs Actual - Random Forest"
  )


#### ---- Hyperparameter Tuning ---- ####

# parallelization for tuning
cores <- parallel::detectCores() - 1
plan("multisession", workers = cores)

# hyperparameter search space for RF
rf_ps <- ps(
  mtry          = p_int(lower = 2,  upper = 10),
  min.node.size = p_int(lower = 10, upper = 100)
)

# inner resampling for tuning
res_inner  <- rsmp("cv", folds = 3)
mes_inner  <- msr("regr.rmse")
terminator <- trm("evals", n_evals = 50)
tuner      <- tnr("random_search")

# autoTuner
rf_at <- AutoTuner$new(
  learner      = rf,
  resampling   = res_inner,
  measure      = mes_inner,
  search_space = rf_ps,
  terminator   = terminator,
  tuner        = tuner
)

#### ---- Benchmark ---- ####

# dummy model
dummy_bench <- lrn("regr.featureless", id = "dummy")

# linear regression
lm_bench <- as_learner(
  po("removeconstants") %>>% po("imputehist") %>>% lrn("regr.lm")
)
lm_bench$id <- "lm"

# elastic net
enet_bench <- as_learner(
  po("removeconstants") %>>% po("imputehist") %>>% po("encode") %>>% enet_learner
)
enet_bench$id <- "elastic_net"

# RF baseline
rf_baseline_bench <- as_learner(po("imputehist") %>>% rf)
rf_baseline_bench$id <- "rf_baseline"

# RF tuned
rf_tuned_bench <- as_learner(po("imputehist") %>>% rf_at)
rf_tuned_bench$id <- "rf_tuned"

# outer 10-fold CV
resampling_outer <- rsmp("cv", folds = 10)

# benchmark grid
design_reg <- benchmark_grid(
  tasks       = task_cabbage,
  learners    = list(dummy_bench, lm_bench, enet_bench, rf_baseline_bench, rf_tuned_bench),
  resamplings = resampling_outer
)

# run benchmark
bm_reg <- benchmark(design_reg)

# results
bm_results <- bm_reg$aggregate(msrs(c("regr.rmse", "regr.mae", "regr.rsq")))
bm_results

# plot
autoplot(bm_reg, type = "boxplot", measure = msr("regr.mae"))

# best for each model
scores_all <- as.data.table(
  bm_reg$score(msrs(c("regr.rmse", "regr.mae", "regr.rsq")))
)

best_per_model <- scores_all[
  ,
  .(
    best_rmse = min(regr.rmse),
    best_mae  = min(regr.mae),
    best_rsq  = max(regr.rsq)
  ),
  by = learner_id
]
best_per_model

# turn off parallelization
plan("sequential")


#### ---- Tuned RF on the predefined train/test split ---- ####

rf_tuned_single <- rf_tuned_bench$clone(deep = TRUE)

# train and predict
rf_tuned_single$train(task_cabbage, splits$train)
pred_tuned <- rf_tuned_single$predict(task_cabbage, splits$test)

# results
measures_tuned <- msrs(c("regr.rmse", "regr.mae", "regr.rsq"))
pred_tuned$score(measures_tuned)

mae_rf_tuned_single  <- pred_tuned$score(msr("regr.mae"))
rmse_rf_tuned_single <- pred_tuned$score(msr("regr.rmse"))
rsq_rf_tuned_single  <- pred_tuned$score(msr("regr.rsq"))

cat("Tuned RF MAE:", round(mae_rf_tuned_single, 3), "\n")

# plot
pdt_tuned <- as.data.table(pred_tuned)
ggplot(pdt_tuned, aes(x = truth, y = response)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(
    x     = "Actual Weight",
    y     = "Predicted Weight",
    title = "Predicted vs Actual - Tuned Random Forest"
  )


#### ---- Variable importance ---- ####

# baseline MAE on the test split
pred_base <- rf_tuned_single$predict(task_cabbage, splits$test)
base_mae  <- pred_base$score(msr("regr.mae"))

# data of the test split
test_dt <- as.data.table(task_cabbage$data(rows = splits$test))

# variables to permute
feature_cols <- setdiff(colnames(test_dt), "Weight")

set.seed(123)
n_reps <- 20

imp_list <- lapply(feature_cols, function(feat) {
  imps <- numeric(n_reps)
  
  for (r in seq_len(n_reps)) {
    dt_perm <- copy(test_dt)
    dt_perm[[feat]] <- sample(dt_perm[[feat]])
    
    pred_perm <- rf_tuned_single$predict_newdata(dt_perm)
    perm_mae  <- pred_perm$score(msr("regr.mae"))
    
    imps[r] <- perm_mae - base_mae
  }
  
  data.table(
    Feature         = feat,
    Importance      = mean(imps),
    Importance_low  = quantile(imps, 0.05),
    Importance_high = quantile(imps, 0.95)
  )
})

var_imp_dt <- rbindlist(imp_list)

# sort by mean importance
var_imp_dt <- var_imp_dt[order(Importance)]
var_imp_dt <- tail(var_imp_dt, n = 15)
var_imp_dt[, Feature := factor(Feature, levels = Feature)]

# plot
ggplot(var_imp_dt, aes(y = Feature, x = Importance)) +
  geom_errorbarh(aes(xmin = Importance_low, xmax = Importance_high),
                 height = 0, linewidth = 0.7) +
  geom_point(size = 2) +
  labs(
    title = "Variable Importance (Tuned RF)",
    x     = "Importance (increase in MAE)",
    y     = NULL
  ) +
  theme_minimal()


#### ---- Theoretical Lower-Bound MAE ---- ####

df_lalique <- copy(df)
df_lalique[, c("Crop", "Sort") := NULL]

# separate features and target
target_col   <- "Weight"
feature_cols2 <- setdiff(names(df_lalique), target_col)

# group by ALL feature columns
grouped <- df_lalique[, .(Weights = list(Weight)), by = feature_cols2]

total_absolute_error <- 0
n <- nrow(df_lalique)

# loop through groups
for (i in seq_len(nrow(grouped))) {
  true_weights  <- unlist(grouped$Weights[[i]])
  median_weight <- median(true_weights)
  total_absolute_error <- total_absolute_error + sum(abs(true_weights - median_weight))
}

weighted_mae <- total_absolute_error / n
cat("Theoretical lower-bound MAE:", round(weighted_mae, 3), "\n")


#### ---- Final Benchmark For All Models ---- ####

scores_mae <- as.data.table(bm_reg$score(msr("regr.mae")))

scores_mae[, Model := factor(
  learner_id,
  levels = c("dummy", "lm", "elastic_net", "rf_baseline", "rf_tuned"),
  labels = c("Dummy", "Linear Regression", "Elastic Net", "RF Baseline", "RF Tuned")
)]

# plot
final_plot <- ggplot(scores_mae, aes(x = Model, y = regr.mae, fill = Model)) +
  geom_boxplot(alpha = 0.8, width = 0.6, outlier.shape = 21) +
  geom_hline(yintercept = weighted_mae, linetype = "dashed", color = "red", linewidth = 1) +
  annotate(
    "text",
    x = 2,
    y = weighted_mae,
    label = paste0("Theoretical lower-bound MAE = ", round(weighted_mae, 3)),
    vjust = -0.6,
    color = "red",
    fontface = "bold",
    size = 3.5
  ) +
  labs(
    title = "10-fold CV MAE Comparison for All Models",
    x     = "Models",
    y     = "MAE"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5),
    axis.title.x    = element_text(face = "bold"),
    axis.title.y    = element_text(face = "bold"),
    legend.position = "none"
  )

print(final_plot)


#### ---- Nutrient Solution Distribution: Top 100 vs All ---- ####

nutrient_col <- "Nutrient_Solution"
top100 <- df[order(-Weight)][1:100]

dist_all <- df[, .N, by = nutrient_col]
setnames(dist_all, nutrient_col, "Nutrient_Solution")
dist_all[, group := "All plants"]
dist_all[, pct := N / sum(N) * 100]

dist_top <- top100[, .N, by = nutrient_col]
setnames(dist_top, nutrient_col, "Nutrient_Solution")
dist_top[, group := "Top 100"]
dist_top[, pct := N / sum(N) * 100]

nutrient_dist <- rbind(dist_all, dist_top, use.names = TRUE)

ggplot(nutrient_dist, aes(x = Nutrient_Solution, y = pct, fill = group)) +
  geom_col(position = "dodge") +
  labs(
    title = "Nutrient Solution Distribution (Top 100 vs All)",
    x     = "Nutrient Solution",
    y     = "Percentage within group",
    fill  = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title  = element_text(face = "bold", hjust = 0.5)
  )


#### ---- Relative Humidity %2 Distribution: Top 100 vs All ---- ####

rh_col2 <- "I_Relative_humidity_(RH)__pct2"
top100 <- df[order(-Weight)][1:100]

rh2_all <- df[,     .(group = "All plants", RH = get(rh_col2))]
rh2_top <- top100[, .(group = "Top 100",    RH = get(rh_col2))]
rh2_dist <- rbind(rh2_all, rh2_top)

ggplot(rh2_dist, aes(x = RH, fill = group)) +
  geom_density(alpha = 0.4, position = "identity") +
  labs(
    title = "Relative Humidity (measurement 2): Top 100 vs All",
    x     = "Relative Humidity measurement 2",
    y     = "Density",
    fill  = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("RH2_distribution_preview.png", dpi = 400, width = 12, height = 6)


#### ---- Relative Humidity %5 Distribution: Top 100 vs All ---- ####

rh_col5 <- "I_Relative_humidity_(RH)__pct5"
top100 <- df[order(-Weight)][1:100]

rh5_all <- df[,     .(group = "All plants", RH = get(rh_col5))]
rh5_top <- top100[, .(group = "Top 100",    RH = get(rh_col5))]
rh5_dist <- rbind(rh5_all, rh5_top)

ggplot(rh5_dist, aes(x = RH, fill = group)) +
  geom_density(alpha = 0.4, position = "identity") +
  labs(
    title = "Relative Humidity (measurement 5): Top 100 vs All",
    x     = "Relative Humidity measurement 5",
    y     = "Density",
    fill  = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("RH5_distribution_preview.png", dpi = 400, width = 12, height = 6)
