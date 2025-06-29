
#### Load Required Libraries ####

# Helper function to install CRAN packages if not present
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

# CRAN packages
cran_packages <- c("pROC", "glmnet", "tidyverse", "missForest", "caret", "MLmetrics", 
                   "randomForest", "xgboost", "mlr3", "mlr3learners", 
                   "mlr3verse", "mlr3tuning", "paradox", "data.table", "haven", "labelled", "missRanger",
                   "smotefamily")

lapply(cran_packages, install_if_missing)

# Load CRAN libraries
library(missRanger)
library(pROC)
library(glmnet)
library(tidyverse)
library(missForest)
library(caret)
library(MLmetrics)
library(randomForest)
library(xgboost)
library(mlr3)
library(mlr3learners)
library(mlr3verse)
library(mlr3tuning)
library(paradox)
library(data.table)
library(haven)
library(labelled)
library(smotefamily)

#### Read data #### 

# Read the data

bes <- read_dta("bes_f2f_2017_v1.5.dta")

# clean the data
vars_to_remove_raw <- c(
  "followup1", "followup2", "intuse", "y48", "finalserialno", "agency",
  "csesmode", "surveymode", "region", "stratum", "stratumlabel", "stratumlabel2",
  "constit_code", "constit_name", "la_ua_code", "la_ua_name", "interview_date", "scan_date",
  "dwelling_type", "total_num_dwel", "total_num_hous", "num_elig_people", "pano",
  "validation_summary", "validated_turnout", "interview_date", "interviewer", "a01", "Q22_CSES", "serial",
  "nr_calls_1st", "sel_wt_capped"
)

# Match exact names in `bes` based on lowercase comparison
vars_to_remove <- names(bes)[tolower(names(bes)) %in% tolower(vars_to_remove_raw)]

# Now clean the data
bes_clean <- bes %>%
  select(
    -starts_with("wt"),
    -starts_with("y48"),
    -any_of(c(vars_to_remove))  # include interviewer too
  )


# Get the labeled dataset
df_label <- bes_clean %>%
  mutate(
    across(where(is.labelled), as_factor)  # for human-readable analysis
  )


# get the labelled vars
labelled_vars <- sapply(bes, function(x) !is.null(attr(x, "labels")))
labelled_vars[labelled_vars == TRUE]

label_info <- lapply(bes[labelled_vars], function(x) attr(x, "labels"))

# get labelled version of the response variable - keep all of this because we extract the y
bes_clean$h1_numeric <- as.numeric(bes_clean$h01)

bes_clean$h1_factor <- recode(
  bes_clean$h1_numeric,
  `-999` = "Not stated",
  `-1`   = "Don't know",
  `0`    = "Growth priority",
  `1`    = "1", 
  `2` = "2", 
  `3` = "3",
  `4` = "4", 
  `5` = "5",
  `6`    = "6", 
  `7` = "7", 
  `8` = "8", 
  `9` = "9",
  `10`   = "Environment priority"
)

bes_clean <- bes_clean %>%
  mutate(across(everything(), ~ ifelse(. == -999, "Not Stated", .))) %>%
  mutate(across(everything(), ~ ifelse(. == -2, "Refused", .))) %>%
  mutate(across(everything(), ~ ifelse(. == -1, "Don't Know", .)))

y <- as.factor(bes_clean$h1_factor)

# remove response from original dataset 
bes_clean <- bes_clean %>%
  select(-h01, -h1_numeric, -h1_factor)

# Drop variables with more than 50% missing
df_filtered_label <- df_label %>%
  select(where(~ mean(is.na(.)) <= 0.5))

# Create missing value indicators
na_cols <- names(df_filtered)[sapply(df_filtered, function(x) any(is.na(x)))]


# add the missing indicator to the labelled filtered set
for (col in na_cols) {
  df_filtered_label[[paste0(col, "_NA")]] <- is.na(df_filtered_label[[col]])
}

test <- df_filtered_label
bes_clean$h01 <- as.factor(y)

add_missing_level <- function(df, vars, missing_label = "Missing") {
  for (var in vars) {
    if (var %in% names(df)) {
      if (!is.factor(df[[var]])) {
        warning(paste("Variable", var, "is not a factor. Skipping."))
        next
      }
      
      # Add "Missing" to factor levels if it's not already there
      if (!(missing_label %in% levels(df[[var]]))) {
        levels(df[[var]]) <- c(levels(df[[var]]), missing_label)
      }
      
      # Replace NAs with the missing label
      df[[var]][is.na(df[[var]])] <- missing_label
    } else {
      warning(paste("Variable", var, "not found in the dataset."))
    }
  }
  return(df)
}

assign_missing_if_condition <- function(df, target_prefix, condition_var, condition_value, missing_label = "Missing") {
  # Find all target variables that start with the specified prefix
  target_vars <- grep(paste0("^", target_prefix), names(df), value = TRUE)
  
  for (var in target_vars) {
    if (var %in% names(df)) {
      # Ensure variable is factor
      df[[var]] <- as.factor(df[[var]])
      
      # Add missing label if it's not already present
      if (!(missing_label %in% levels(df[[var]]))) {
        levels(df[[var]]) <- c(levels(df[[var]]), missing_label)
      }
      
      # Assign "Missing" where the condition is met and the value is NA
      condition_rows <- is.na(df[[var]]) & df[[condition_var]] == condition_value
      df[[var]][condition_rows] <- missing_label
    }
  }
  return(df)
}

# Start with your manually specified variables
vars_to_patch <- c("b02", "b05", "d04")

conditions_df <- tibble::tibble(
  target_prefix = c("k14", "y07", "y22", "y23", "y22", "y23", "y22", "y23", "y22", "y23", "y22", "y23", "y22", "y23"),
  condition_var  = c("k13", "y06", "y17", "y17", "y17", "y17", "y17", "y17", "y17", "y17", "y17", "y17", "y17", "y17"),
  condition_value = c(
    "No",
    "No religion",
    "Working full time - self-employed (30+ hours)",
    "Working full time - self-employed (30+ hours)",
    "Working part time - self-employed (8-29 hours)",
    "Working part time - self-employed (8-29 hours)",
    "Retired from paid work",
    "Retired from paid work", 
    "Looking after the family or home",
    "Looking after the family or home", 
    "Not working because long-term sick or disabled",
    "Not working because long-term sick or disabled", 
    "A full time student or pupil",
    "A full time student or pupil"
  )
)

for (i in seq_len(nrow(conditions_df))) {
  test <- assign_missing_if_condition(
    df = test,
    target_prefix = conditions_df$target_prefix[i],
    condition_var = conditions_df$condition_var[i],
    condition_value = conditions_df$condition_value[i]
  )
}

# Add all variables starting with "b06"
b06_vars <- grep("^b06", names(df_filtered_label), value = TRUE)

# Combine the lists
vars_to_patch <- unique(c(vars_to_patch, b06_vars))
test <- add_missing_level(test, vars_to_patch)

#### Missing Values ####

# Check the percentage of missing values in each variable
missing_pct <- sapply(test, function(x) mean(is.na(x)))
high_missing_vars <- names(missing_pct[missing_pct > 0.5])

# Numeric variables
numeric_vars <- names(bes_clean)[sapply(bes_clean, is.numeric)]

# Categorical variables
categorical_vars <- names(bes_clean)[sapply(bes_clean, function(x) is.factor(x) || is.character(x))]

# character vars
char_vars <- names(df_filtered)[sapply(df_filtered, is.character)]

#### Missing Values ####

# check for missing values now
missing_values_train <- sapply(bes_clean, function(x) sum(is.na(x))) %>%
  enframe(name = "Variable", value = "NA_Count") %>%
  filter(NA_Count > 0) %>%
  arrange(desc(NA_Count)) %>%
  filter(NA_Count > 0)

# Fix the type of variables in some of these
test$Age <- as.numeric(test$Age)

# Now impute with missRanger
test <- test%>% # remove response variable from here
  select(-h01) %>%
  mutate(as.numeric(Age))

df_imputed <- missRanger(test, num.trees = 100)

#### Split the data #### 

# now split into training, validation, and test data

train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- df_imputed[train_index, ]
X_temp <- df_imputed[-train_index, ]
y_train <- y[train_index]
y_temp <- y[-train_index]

val_index <- createDataPartition(y_temp, p = 0.5, list = FALSE)
X_val <- X_temp[val_index, ]
X_test <- X_temp[-val_index, ]

y_val <- y_temp[val_index]
y_test <- y_temp[-val_index]


## Compare distributions of imputed vs. numerical variables 
vars_to_compare <- c("a02", "b05", "b0601", "f01_6", "g01_4")

compare_categorical <- function(var) {
  original <- test %>%
    select({{ var }}) %>%
    mutate(dataset = "Original") %>%
    rename(value = {{ var }})
  
  imputed <- df_imputed %>%
    select({{ var }}) %>%
    mutate(dataset = "Imputed") %>%
    rename(value = {{ var }})
  
  combined <- bind_rows(original, imputed)
  
  combined$value <- factor(combined$value, levels = union(levels(original$value), levels(imputed$value)))
  
  ggplot(combined, aes(x = as.factor(value), fill = dataset)) +
    geom_bar(position = "dodge") +
    labs(title = paste("Distribution of", var),
         x = var, y = "Count") +
    theme_minimal()
}

compare_numeric <- function(var) {
  original <- df_filtered_label %>%
    select({{ var }}) %>%
    mutate(dataset = "Original") %>%
    rename(value = {{ var }})
  
  imputed <- df_imputed %>%
    select({{ var }}) %>%
    mutate(dataset = "Imputed") %>%
    rename(value = {{ var }})
  
  combined <- bind_rows(original, imputed)
  
  ggplot(combined, aes(x = as.numeric(value), fill = dataset)) +
    geom_density(alpha = 0.4) +
    labs(title = paste("Distribution of", var),
         x = var, y = "Density") +
    theme_minimal()
}

compare_categorical("y22")

##### FUNCTIONS FOR LATER #####  
minimal_impute <- function(df) {
  for (col in names(df)) {
    if (any(is.na(df[[col]]))) {
      if (is.numeric(df[[col]])) {
        df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
      } else if (is.factor(df[[col]]) || is.character(df[[col]])) {
        mode_val <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
        df[[col]][is.na(df[[col]])] <- mode_val
      }
    }
  }
  return(df)
}

prepare_validation_matrix <- function(X_val, ref_colnames) {
  val_imputed <- minimal_impute(X_val)
  val_encoded <- model.matrix(~ . - 1, data = val_imputed)
  return(align_columns(val_encoded, ref_colnames))
}

align_columns <- function(new_data, ref_cols) {
  missing_cols <- setdiff(ref_cols, colnames(new_data))
  for (col in missing_cols) new_data[[col]] <- 0
  new_data <- new_data[, ref_cols, drop = FALSE]
  return(as.matrix(new_data))
}

# ---- One-Hot Encoding Function
prepare_matrix <- function(df) {
  model.matrix(~ . - 1, data = df)
}

# ---- Training XGBoost Function
train_xgboost <- function(X, y, params = list(), nrounds = 500, X_val = NULL, y_val = NULL, early_stopping_rounds = 10) {
  dtrain <- xgb.DMatrix(data = X, label = y)
  
  watchlist <- list(train = dtrain)
  if (!is.null(X_val) && !is.null(y_val)) {
    dval <- xgb.DMatrix(data = X_val, label = y_val)
    watchlist$eval <- dval
  }
  
  xgb.train(
    data = dtrain,
    params = params,
    nrounds = nrounds,
    watchlist = watchlist,
    early_stopping_rounds = early_stopping_rounds,
    print_every_n = 10,
    verbose = 1
  )
}

# ---- BUILD MODELS ----- #

# Random Forest
rf_model <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 1000,
  mtry = 3,
  importance = TRUE
)

# Predict on validation set
rf_pred_val <- predict(rf_model, newdata = X_val, type = "response")

# Confusion Matrix
conf_mat <- confusionMatrix(rf_pred_val, y_val)
print(conf_mat)

# Macro F1 Score
f1_rf<- mean(conf_mat$byClass[, "F1"], na.rm = TRUE)

#### XGBoost Pipeline ####

# ---- Label Preparation
# Make sure the full y is a factor
y <- factor(y)  # this defines the correct levels: 0 to 10 + Don't know

# Subset factor-consistent labels
y_train <- y[train_index]
y_val   <- y_temp[val_index]
y_test  <- y_temp[-val_index]

# Convert all to 0-based numeric safely
y_train_num <- as.integer(y_train) - 1
y_val_num   <- as.integer(y_val) - 1
y_test_num  <- as.integer(y_test) - 1

# ---- XGBoost Parameters
params <- list(
  objective = "multi:softprob",
  num_class = length(unique(y_train_num)),
  eval_metric = "mlogloss"
)

# ---- Training

# Remove rows with missing labels (if needed)
keep <- which(!is.na(y_train_num))
df_train <- X_train[keep, ]
y_train_num <- y_train_num[keep]

# Minimal imputation on training data
df_train <- minimal_impute(df_train)

# Create model matrix for training
X_train_mat <- prepare_matrix(df_train)
train_colnames <- colnames(X_train_mat)

# ---- Validation Preparation
X_val_mat <- prepare_validation_matrix(X_val, colnames(X_train_mat))
dval <- xgb.DMatrix(data = X_val_mat)

# Model
model <- train_xgboost(
  X = X_train_mat,
  y = y_train_num,
  params = params,
  X_val = X_val_mat,
  y_val = y_val_num,
  nrounds = 1000,
  early_stopping_rounds = 25
)

# Predict with best number of boosting rounds
pred_prob <- predict(model, newdata = dval, ntreelimit = model$best_iteration)

# Get class predictions from probability matrix
pred_matrix <- matrix(pred_prob, ncol = params$num_class, byrow = TRUE)
pred_class <- max.col(pred_matrix) - 1

# Confusion matrix and macro F1 evaluation
conf_mat <- confusionMatrix(
  factor(pred_class, levels = 0:(params$num_class - 1)),
  factor(y_val_num, levels = 0:(params$num_class - 1))
)

print(conf_mat)

# Compute macro F1
f1_xgb <- mean(conf_mat$byClass[, "F1"], na.rm = TRUE)
cat("Macro F1 Score:", round(f1_xgb, 3), "\n")
print(conf_mat$byClass[, "F1"])

#### ---- Collapse into 3-Class Group ----

# Recode raw labels into grouped categories
y_raw <- as.character(y)

# Now group them
y_grouped <- case_when(
  y_raw %in% c("Not stated", NA)         ~ "Not Stated",  # (optional, filtered later)
  y_raw == "Don't know"                  ~ "Don't Know",
  y_raw %in% c("0", "1", "2", "3")       ~ "0-3",
  y_raw %in% c("4", "5", "6")            ~ "4-6",
  y_raw %in% c("7", "8", "9", "10")      ~ "7-10",
  y_raw == "Growth priority"            ~ "0-3",
  y_raw == "Environment priority"       ~ "7-10",
  TRUE                                   ~ NA_character_
)

# Make it a factor with predefined level order
# Define 4-class numeric mapping: 0 = Don't Know, 1 = 0-3, 2 = 4-6, 3 = 7-10
grouped_levels <- c("Don't Know", "0-3", "4-6", "7-10")
y_grouped <- factor(y_grouped, levels = grouped_levels)  # ensure correct order

# 3. Subset labels to splits + convert to 0-based numeric
to_numeric_labels <- function(fct) as.integer(fct) - 1

y_train_grp     <- y_grouped[train_index]
y_val_grp       <- y_grouped[val_index]
y_test_grp      <- y_grouped[-val_index]

y_train_num_grp <- to_numeric_labels(y_train_grp)
y_val_num_grp   <- to_numeric_labels(y_val_grp)
y_test_num_grp  <- to_numeric_labels(y_test_grp)

# ---- XGBoost Parameters for Grouped Model
params_grp <- list(
  objective = "multi:softprob",
  num_class = length(levels(y_train_grp)),
  eval_metric = "mlogloss"
)

# ---- Preprocessing

# Minimal Imputation (reusing function)
X_train_grp <- minimal_impute(X_train)

# One-hot encoding (reusing function)
X_train_mat_grp <- prepare_matrix(X_train_grp)
train_colnames_grp <- colnames(X_train_mat_grp)

# ---- Validation Preparation for Grouped Model
X_val_mat_grp <- prepare_validation_matrix(X_val, train_colnames_grp)

# ---- Train Grouped Model 
model_grp <- train_xgboost(
  X = X_train_mat_grp,
  y = y_train_num_grp,
  params = params_grp,
  X_val = X_val_mat_grp,
  y_val = y_val_num_grp,
  nrounds = 1000,
  early_stopping_rounds = 25
)

# Predict 
dval_grp <- xgb.DMatrix(data = X_val_mat_grp)
pred_prob_grp <- predict(model_grp, newdata = dval_grp)

# Reshape to Class Probabilities
pred_matrix_grp <- matrix(pred_prob_grp, ncol = params_grp$num_class, byrow = TRUE)

# Predicted Class (0-based indexing
pred_class_grp <- max.col(pred_matrix_grp) - 1

# Evaluation 
conf_mat_grp <- confusionMatrix(
  factor(pred_class_grp, levels = 0:(params_grp$num_class - 1)),
  factor(y_val_num_grp, levels = 0:(params_grp$num_class - 1))
)

# Print results 
print(conf_mat_grp)

# Macro F1 Score
f1_xgb_grp <- mean(conf_mat_grp$byClass[, "F1"], na.rm = TRUE)
cat("Macro F1 Score (Grouped):", round(f1_xgb_grp, 3), "\n")

#### Grid Search XGBoost 
num_classes <- length(unique(y_train_num_grp))

grid <- expand.grid(
  max_depth = c(3, 5),
  eta = c(0.05, 0.1),
  min_child_weight = c(1, 3),
  subsample = c(0.8),
  colsample_bytree = c(0.8)
)

macro_f1_eval <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  num_class <- length(unique(labels))
  
  pred_matrix <- matrix(preds, ncol = num_class, byrow = TRUE)
  pred_labels <- max.col(pred_matrix) - 1
  
  f1_scores <- sapply(0:(num_class - 1), function(class) {
    tp <- sum(pred_labels == class & labels == class)
    fp <- sum(pred_labels == class & labels != class)
    fn <- sum(pred_labels != class & labels == class)
    denom <- (2 * tp + fp + fn)
    if (denom == 0) return(NA)
    return(2 * tp / denom)
  })
  
  f1_macro <- mean(f1_scores, na.rm = TRUE)
  return(list(metric = "macro_f1", value = f1_macro))
}

# data for the loop
dtrain_grp <- xgb.DMatrix(data = X_train_mat_grp, label = y_train_num_grp)

best_f1 <- -Inf
best_params <- list()
best_nrounds <- NULL

for (i in 1:nrow(grid)) {
  cat("\nRunning grid config", i, "of", nrow(grid), "...\n")
  
  params <- list(
    booster = "gbtree",
    objective = "multi:softprob",
    num_class = num_classes,
    max_depth = grid$max_depth[i],
    eta = grid$eta[i],
    min_child_weight = grid$min_child_weight[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = dtrain_grp,
    nrounds = 200,
    nfold = 5,
    early_stopping_rounds = 10,
    feval = macro_f1_eval,
    maximize = TRUE,
    verbose = 0,
    stratified = TRUE
  )
  
  f1_vec <- cv$evaluation_log$test_macro_f1_mean
  best_f1_current <- max(f1_vec, na.rm = TRUE)
  best_iter_current <- which.max(f1_vec)
  
  cat("Best Macro F1:", round(best_f1_current, 4), "\n")
  
  if (best_f1_current > best_f1) {
    best_f1 <- best_f1_current
    best_params <- params
    best_nrounds <- best_iter_current
  }
}

final_model_grp <- xgb.train(
  params = best_params,
  data = dtrain_grp,
  nrounds = best_nrounds
)

# Validation Prediction

dval <- xgb.DMatrix(data = X_val_mat_grp)
val_preds <- predict(final_model_grp, newdata = dval)
val_matrix <- matrix(val_preds, ncol = num_classes, byrow = TRUE)
val_pred_labels <- max.col(val_matrix) - 1

# Confusion matrix
conf_mat_val <- confusionMatrix(
  factor(val_pred_labels, levels = 0:(num_classes - 1)),
  factor(y_val_num_grp, levels = 0:(num_classes - 1))
)
print(conf_mat_val)

# Macro F1
f1_xgb_grid_grp <- mean(conf_mat_val$byClass[, "F1"], na.rm = TRUE)
cat("F1 (Best Standard XGBoost Model):", round(f1_xgb_grid_grp, 3), "\n")

# --- SMOTE Preparation ---
X_train_ohe <- model.matrix(~ . - 1, data = X_train_grp)
train_smote_input <- as.data.frame(X_train_ohe)
train_labels <- as.factor(y_train_num_grp)

smote_result <- SMOTE(
  X = train_smote_input,
  target = train_labels,
  K = 5,
  dup_size = 0
)

X_smote <- smote_result$data[, !names(smote_result$data) %in% "class"]
y_smote <- as.factor(smote_result$data$class)
y_smote_num <- as.integer(y_smote) - 1
cat("SMOTE Label Distribution:\n")
print(table(y_smote))

dtrain_smote <- xgb.DMatrix(data = as.matrix(X_smote), label = y_smote_num)

# --- Validation Preparation ---
X_val_mat <- prepare_validation_matrix(X_val, colnames(X_smote))
dval_smote <- xgb.DMatrix(data = X_val_mat, label = y_val_num_grp)

# --- XGBoost Parameters ---
params_smote <- list(
  booster = "gbtree",
  objective = "multi:softprob",
  num_class = length(unique(y_smote_num)),
  eval_metric = "mlogloss",
  max_depth = 4,
  eta = 0.1
)

# --- Train + Predict ---
model_smote <- xgb.train(
  params = params_smote,
  data = dtrain_smote,
  nrounds = 200,
  watchlist = list(train = dtrain_smote, eval = dval_smote),
  early_stopping_rounds = 10,
  verbose = 1
)

pred_matrix <- matrix(predict(model_smote, newdata = dval_smote), 
                      ncol = params_smote$num_class, byrow = TRUE)
pred_classes <- max.col(pred_matrix) - 1

conf_mat_smote <- confusionMatrix(
  factor(pred_classes, levels = 0:(params_smote$num_class - 1)),
  factor(y_val_num_grp, levels = 0:(params_smote$num_class - 1))
)

print(conf_mat_smote)
f1_smote <- mean(conf_mat_smote$byClass[, "F1"], na.rm = TRUE)
cat("Validation Macro F1 (SMOTE model):", round(f1_smote, 3), "\n")

# --- Grid Search Function ---
macro_f1_score <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  num_class <- length(unique(labels))
  pred_matrix <- matrix(preds, ncol = num_class, byrow = TRUE)
  pred_labels <- max.col(pred_matrix) - 1
  
  f1_scores <- sapply(0:(num_class - 1), function(class) {
    tp <- sum(pred_labels == class & labels == class)
    fp <- sum(pred_labels == class & labels != class)
    fn <- sum(pred_labels != class & labels == class)
    denom <- (2 * tp + fp + fn)
    if (denom == 0) return(NA)
    2 * tp / denom
  })
  
  list(metric = "macro_f1", value = mean(f1_scores, na.rm = TRUE))
}

# --- Grid Search Loop ---
best_f1 <- -Inf
best_params <- list()
best_model <- NULL

for (i in 1:nrow(grid)) {
  cat("\n Running grid config", i, "of", nrow(grid), "...\n")
  params <- list(
    booster = "gbtree",
    objective = "multi:softprob",
    num_class = length(unique(y_smote_num)),
    max_depth = grid$max_depth[i],
    eta = grid$eta[i],
    min_child_weight = grid$min_child_weight[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = dtrain_smote,
    nrounds = 200,
    nfold = 5,
    early_stopping_rounds = 10,
    feval = macro_f1_score,
    maximize = TRUE,
    verbose = 0
  )
  
  best_macro_f1 <- max(cv$evaluation_log$test_macro_f1_mean, na.rm = TRUE)
  cat("Best Macro F1:", round(best_macro_f1, 4), "\n")
  
  if (best_macro_f1 > best_f1) {
    best_f1 <- best_macro_f1
    best_params <- params
    best_model <- cv
  }
}

# --- Train Final Model w/ Best Grid Parameters ---
final_model_smote <- xgb.train(
  params = best_params,
  data = dtrain_smote,
  nrounds = best_model$best_iteration,
  verbose = 1
)

# --- Predict on Validation ---
val_probs <- predict(final_model_smote, newdata = dval_smote)
val_preds <- max.col(matrix(val_probs, ncol = best_params$num_class, byrow = TRUE)) - 1

conf_mat_val <- confusionMatrix(
  factor(val_preds, levels = 0:(best_params$num_class - 1)),
  factor(y_val_num_grp, levels = 0:(best_params$num_class - 1))
)

print(conf_mat_val)
f1_smote_grid <- mean(conf_mat_val$byClass[, "F1"], na.rm = TRUE)
cat("Validation Macro F1 (Best SMOTE Model w/ Grid):", round(f1_smote_grid, 3), "\n")

#### Ordinal Regression Pipeline (XGBoost) #####
# --- 1. Map Grouped Labels to Numeric Midpoints ---
label_map <- function(grouped) {
  case_when(
    grouped == "0-3"    ~ 1.5,
    grouped == "4-6"    ~ 5.0,
    grouped == "7-10"   ~ 8.5,
    TRUE                ~ NA_real_  # includes "Don't Know" or NA
  )
}

y_grouped_numeric <- label_map(y_grouped)

# Filter out NA (i.e., "Don't Know")
y_train_numeric_full <- y_grouped_numeric[train_index]
X_train_full <- X_train[!is.na(y_train_numeric_full), ]
y_train_full <- y_train_numeric_full[!is.na(y_train_numeric_full)]

# Impute and encode
X_train_full_imputed <- minimal_impute(X_train_full)
X_train_mat_reg <- model.matrix(~ . - 1, data = X_train_full_imputed)
train_colnames_reg <- colnames(X_train_mat_reg)

# Build DMatrix
dtrain_reg <- xgb.DMatrix(data = X_train_mat_reg, label = y_train_full)

# Prepare full validation set (do not exclude rows)
X_val_mat <- prepare_validation_matrix(X_val, train_colnames_reg)
dval_full <- xgb.DMatrix(data = X_val_mat)

# Track labels (with NAs included for later filtering)
y_val_reg <- y_grouped_numeric[val_index]

# --- 5. Train XGBoost Model ---
params_reg <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 4,
  eval_metric = "rmse"
)

model_reg <- xgb.train(
  params = params_reg,
  data = dtrain_reg,
  nrounds = 200,
  early_stopping_rounds = 10,
  watchlist = list(train = dtrain_reg),
  verbose = 1
)

# --- 6. Predict on full validation set (292 rows) ---
val_pred_cont <- predict(model_reg, newdata = dval_full)

# Filter valid (non-"Don't Know") rows
valid_idx <- which(!is.na(y_val_reg))
val_pred_known <- val_pred_cont[valid_idx]
y_val_known <- y_grouped[val_index][valid_idx]

# Grid search thresholds
lower_bounds <- seq(2.5, 4.0, by = 0.1)
upper_bounds <- seq(5.5, 7.5, by = 0.1)
threshold_grid <- expand.grid(lower = lower_bounds, upper = upper_bounds)

f1_scores <- apply(threshold_grid, 1, function(thresh) {
  pred_binned <- cut(val_pred_known, breaks = c(-Inf, thresh[1], thresh[2], Inf),
                     labels = c("0-3", "4-6", "7-10"))
  conf <- confusionMatrix(pred_binned, y_val_known)
  mean(conf$byClass[, "F1"], na.rm = TRUE)
})

# Select best thresholds
best_idx <- which.max(f1_scores)
best_thresh <- threshold_grid[best_idx, ]

val_pred_binned <- cut(
  val_pred_cont,
  breaks = c(-Inf, best_thresh$lower, best_thresh$upper, Inf),
  labels = c("0-3", "4-6", "7-10")
)

# Evaluate with best thresholds
pred_binned_known <- cut(
  val_pred_known,
  breaks = c(-Inf, best_thresh$lower, best_thresh$upper, Inf),
  labels = c("0-3", "4-6", "7-10")
)

# Confusion matrix on non-NA labels
conf_mat_ordreg <- confusionMatrix(pred_binned_known, y_val_known)

# Print the matrix
print(conf_mat_ordreg$table)

# Class-wise F1
f1_per_class <- conf_mat_ordreg$byClass[, "F1"]
print(round(f1_per_class, 3))


# Compute and print class-wise F1 scores
f1_per_class <- conf_mat_ordreg$byClass[, "F1"]
names(f1_per_class) <- colnames(conf_mat_ordreg$table)  # label with class names
print(round(f1_per_class, 3))

# Compute and print macro F1 (excluding NA, e.g., from "Don't Know" if not predicted)
f1_ordreg <- mean(f1_per_class, na.rm = TRUE)
cat("\n Validation Macro F1 (Ordinal Regression with 'Don't Know'):", round(f1_ordreg, 3), "\n")

#### Ordinal Regression with Quantile-Based Thresholding ####
# Create grid of quantile thresholds
quantile_grid <- expand.grid(
  lower_q = seq(0.20, 0.50, by = 0.05),
  upper_q = seq(0.60, 0.85, by = 0.05)
)
quantile_grid <- subset(quantile_grid, upper_q > lower_q)

# Prepare Validation Set 
X_val_mat <- prepare_validation_matrix(X_val, colnames(X_train_mat_reg))
dval <- xgb.DMatrix(data = X_val_mat)

# Generate predictions
val_pred_cont <- predict(model_reg, newdata = dval)

# Actual labels
y_val_factor <- factor(y_val_grp[1:length(val_pred_cont)], levels = c("0-3", "4-6", "7-10"))

# Initialize Results Storage (Preallocated
f1_scores_q <- data.frame(
  lower_q = quantile_grid$lower_q,
  upper_q = quantile_grid$upper_q,
  macro_f1 = NA_real_
)

# ---- Evaluate Each Quantile Threshold Pair ----
for (i in seq_len(nrow(quantile_grid))) {
  q_lower <- f1_scores_q$lower_q[i]
  q_upper <- f1_scores_q$upper_q[i]
  
  lower_val <- quantile(val_pred_cont, probs = q_lower)
  upper_val <- quantile(val_pred_cont, probs = q_upper)
  
  pred_binned <- cut(
    val_pred_cont,
    breaks = c(-Inf, lower_val, upper_val, Inf),
    labels = c("0-3", "4-6", "7-10"),
    right = TRUE
  )
  
  conf_mat <- confusionMatrix(pred_binned, y_val_factor)
  f1_scores_q$macro_f1[i] <- mean(conf_mat$byClass[, "F1"], na.rm = TRUE)
}

# ---- Extract and Evaluate Best Quantile Thresholds ----

# 1. Identify best-performing threshold pair from the grid
best_idx <- which.max(f1_scores_q$macro_f1)
best_quantiles <- f1_scores_q[best_idx, ]

# 2. Convert quantiles to numeric thresholds based on predictions
best_lower_val <- quantile(val_pred_cont, probs = best_quantiles$lower_q)
best_upper_val <- quantile(val_pred_cont, probs = best_quantiles$upper_q)

# 3. Store and report validation macro F1 from grid
temp_ordreg_quantile <- best_quantiles$macro_f1
cat("Best Quantile Thresholds:\n")
print(best_quantiles)

cat("\nConverted Prediction Thresholds:\n")
cat("Lower:", round(best_lower_val, 4), "\n")
cat("Upper:", round(best_upper_val, 4), "\n")
cat("Validation Macro F1 (Quantile Grid Search):", round(temp_ordreg_quantile, 3), "\n")

# ---- Final Evaluation with Best Quantile Thresholds ----

# 4. Apply thresholds to bin continuous predictions
val_pred_binned <- cut(
  val_pred_cont,
  breaks = c(-Inf, best_lower_val, best_upper_val, Inf),
  labels = c("0-3", "4-6", "7-10"),
  right = TRUE
)

# 5. Format ground truth labels (ensure length match)
y_val_factor <- factor(y_val_grp[1:length(val_pred_binned)], levels = c("0-3", "4-6", "7-10"))

#### ---- Quantile-Informed Binning (Grid Search) ---- ####

# Step 1: Define grid of quantile thresholds
lower_qs <- seq(0.25, 0.45, by = 0.025)
upper_qs <- seq(0.55, 0.75, by = 0.025)
quantile_grid <- expand.grid(lower_q = lower_qs, upper_q = upper_qs)
quantile_grid <- subset(quantile_grid, upper_q > lower_q)  # valid splits only

# Step 2: Prepare storage
f1_results <- data.frame(lower_q = numeric(), upper_q = numeric(), macro_f1 = numeric())

# Step 3: True labels for evaluation
y_val_factor <- factor(y_val_grp, levels = c("0-3", "4-6", "7-10"))

# Step 4: Loop through threshold combinations
for (i in 1:nrow(quantile_grid)) {
  lower_thresh <- quantile(val_pred_cont, probs = quantile_grid$lower_q[i])
  upper_thresh <- quantile(val_pred_cont, probs = quantile_grid$upper_q[i])
  
  pred_binned <- cut(
    val_pred_cont,
    breaks = c(-Inf, lower_thresh, upper_thresh, Inf),
    labels = c("0-3", "4-6", "7-10"),
    right = TRUE
  )
  
  conf <- confusionMatrix(pred_binned, y_val_factor)
  f1 <- mean(conf$byClass[, "F1"], na.rm = TRUE)
  
  f1_results[i, ] <- c(quantile_grid$lower_q[i], quantile_grid$upper_q[i], f1)
}

# Step 5: Select best thresholds
best_row <- f1_results[which.max(f1_results$macro_f1), ]
best_lower_q <- best_row$lower_q
best_upper_q <- best_row$upper_q

# Get actual threshold values
best_lower_val <- quantile(val_pred_cont, probs = best_lower_q)
best_upper_val <- quantile(val_pred_cont, probs = best_upper_q)

cat("Best Quantile Thresholds:\n")
cat("Lower Quantile:", best_lower_q, "(", round(best_lower_val, 3), ") | ",
    "Upper Quantile:", best_upper_q, "(", round(best_upper_val, 3), ")\n")

# Step 6: Final Evaluation
val_pred_binned <- cut(
  val_pred_cont,
  breaks = c(-Inf, best_lower_val, best_upper_val, Inf),
  labels = c("0-3", "4-6", "7-10"),
  right = TRUE
)

conf_mat_val <- confusionMatrix(val_pred_binned, y_val_factor)
print(conf_mat_val)

# Step 7: Compute Macro F1
f1_quint_bin <- mean(conf_mat_val$byClass[, "F1"], na.rm = TRUE)
cat("Validation Macro F1 (Quantile Grid Binning):", round(f1_quint_bin, 3), "\n")

#### XGBoost SMOTE -- Without DK ####
# Step 1 
# y_grouped has all labels as factor("Don't Know", "0–3", "4–6", "7–10")
keep_idx <- which(y_grouped != "Don't Know" & !is.na(y_grouped))

# Filter everything
y_grouped_nodk <- droplevels(y_grouped[keep_idx])
X_nodk <- df_imputed[keep_idx, ]  # Use your cleaned/imputed data frame

# STEP 2: Re-split data without "don't know' 
train_index_nodk <- createDataPartition(y_grouped_nodk, p = 0.8, list = FALSE)

X_train_nodk <- X_nodk[train_index_nodk, ]
X_temp_nodk <- X_nodk[-train_index_nodk, ]

y_train_nodk <- y_grouped_nodk[train_index_nodk]
y_temp_nodk <- y_grouped_nodk[-train_index_nodk]

val_index_nodk <- createDataPartition(y_temp_nodk, p = 0.5, list = FALSE)

X_val_nodk <- X_temp_nodk[val_index_nodk, ]
X_test_nodk <- X_temp_nodk[-val_index_nodk, ]

y_val_nodk <- y_temp_nodk[val_index_nodk]
y_test_nodk <- y_temp_nodk[-val_index_nodk]

# Step 3: One-hot encode
to_numeric <- function(fct) as.integer(fct) - 1

y_train_num_nodk <- to_numeric(y_train_nodk)
y_val_num_nodk   <- to_numeric(y_val_nodk)
y_test_num_nodk  <- to_numeric(y_test_nodk)

# Step 4: Impute and encode
X_train_nodk_imputed <- minimal_impute(X_train_nodk)
X_train_mat_nodk <- prepare_matrix(X_train_nodk_imputed)
train_colnames_nodk <- colnames(X_train_mat_nodk)

X_val_mat_nodk <- prepare_validation_matrix(X_val_nodk, train_colnames_nodk)
dval_nodk <- xgb.DMatrix(data = X_val_mat_nodk, label = y_val_num_nodk)

# Step 5: Build DMatrix
params_nodk <- list(
  objective = "multi:softprob",
  num_class = length(levels(y_grouped_nodk)),
  eval_metric = "mlogloss",
  eta = 0.1,
  max_depth = 4
)

dtrain_nodk <- xgb.DMatrix(data = X_train_mat_nodk, label = y_train_num_nodk)

model_xgb_nodk <- xgb.train(
  params = params_nodk,
  data = dtrain_nodk,
  nrounds = 1000,
  early_stopping_rounds = 25,
  watchlist = list(train = dtrain_nodk, eval = dval_nodk),
  verbose = 1
)

# Predict
pred_probs_nodk <- predict(model_xgb_nodk, newdata = dval_nodk)
pred_matrix_nodk <- matrix(pred_probs_nodk, ncol = params_nodk$num_class, byrow = TRUE)
pred_class_nodk <- max.col(pred_matrix_nodk) - 1

# Evaluate
conf_mat_nodk <- confusionMatrix(
  factor(pred_class_nodk, levels = 0:(params_nodk$num_class - 1)),
  factor(y_val_num_nodk, levels = 0:(params_nodk$num_class - 1))
)

f1_nodk <- mean(conf_mat_nodk$byClass[, "F1"], na.rm = TRUE)
cat("Macro F1 (XGBoost w/o Don't Know):", round(f1_nodk, 3), "\n")
print(conf_mat_nodk$byClass[, "F1"])

##### No DK  + SMOTE ####
# One-hot encode training features
X_train_ohe_nodk <- model.matrix(~ . - 1, data = X_train_nodk)
train_smote_input <- as.data.frame(X_train_ohe_nodk)
train_labels <- as.factor(y_train_num_nodk)

# Apply SMOTE
smote_result_nodk <- SMOTE(
  X = train_smote_input,
  target = train_labels,
  K = 5,
  dup_size = 0  # adjust if you want more synthetic examples
)

X_smote_nodk <- smote_result_nodk$data[, !names(smote_result_nodk$data) %in% "class"]
y_smote_nodk <- as.factor(smote_result_nodk$data$class)
y_smote_num_nodk <- as.integer(y_smote_nodk) - 1

cat("SMOTE Label Distribution:\n")
print(table(y_smote_nodk))

dtrain_smote_nodk <- xgb.DMatrix(data = as.matrix(X_smote_nodk), label = y_smote_num_nodk)

# Match val features to SMOTE features
X_val_mat_nodk <- prepare_validation_matrix(X_val_nodk, colnames(X_smote_nodk))
dval_smote_nodk <- xgb.DMatrix(data = X_val_mat_nodk, label = y_val_num_nodk)

params_smote_nodk <- list(
  booster = "gbtree",
  objective = "multi:softprob",
  num_class = length(unique(y_smote_num_nodk)),
  eval_metric = "mlogloss",
  eta = 0.1,
  max_depth = 4
)

model_smote_nodk <- xgb.train(
  params = params_smote_nodk,
  data = dtrain_smote_nodk,
  nrounds = 200,
  watchlist = list(train = dtrain_smote_nodk, eval = dval_smote_nodk),
  early_stopping_rounds = 10,
  verbose = 1
)

pred_matrix_smote_nodk <- matrix(
  predict(model_smote_nodk, newdata = dval_smote_nodk),
  ncol = params_smote_nodk$num_class,
  byrow = TRUE
)

pred_classes_smote_nodk <- max.col(pred_matrix_smote_nodk) - 1

conf_mat_smote_nodk <- confusionMatrix(
  factor(pred_classes_smote_nodk, levels = 0:(params_smote_nodk$num_class - 1)),
  factor(y_val_num_nodk, levels = 0:(params_smote_nodk$num_class - 1))
)

f1_smote_nodk <- mean(conf_mat_smote_nodk$byClass[, "F1"], na.rm = TRUE)

cat("Macro F1 (XGBoost + SMOTE, No Don't Know):", round(f1_smote_nodk, 3), "\n")
print(round(conf_mat_smote_nodk$byClass[, "F1"], 3))

# Extract per-class F1 scores
f1_class <- round(conf_mat_smote_nodk$byClass[, "F1"], 3)

# Compute macro F1
f1_macro <- round(mean(f1_class, na.rm = TRUE), 3)

# Create data frame
f1_df <- data.frame(
  Class = c("Growth priority", "No strong preference", "Environment priority", "Macro Avg"),
  `F1 Score` = c(f1_class, f1_macro)
)

kable(f1_df, caption = "F1 Scores by Class and Macro Average (SMOTE model, no 'Don't Know')")
saveRDS(conf_mat_smote_nodk, "conf_mat_smote_nodk2.rds")

#### Grid Search Without DK + SMOTE ####
# Define grid of hyperparameters
grid <- expand.grid(
  max_depth = c(3, 4, 5),
  eta = c(0.05, 0.1),
  min_child_weight = c(1, 3),
  subsample = c(0.8),
  colsample_bytree = c(0.8)
)

macro_f1_eval <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  num_class <- length(unique(labels))
  
  pred_matrix <- matrix(preds, ncol = num_class, byrow = TRUE)
  pred_labels <- max.col(pred_matrix) - 1
  
  f1_scores <- sapply(0:(num_class - 1), function(class) {
    tp <- sum(pred_labels == class & labels == class)
    fp <- sum(pred_labels == class & labels != class)
    fn <- sum(pred_labels != class & labels == class)
    denom <- (2 * tp + fp + fn)
    if (denom == 0) return(NA)
    2 * tp / denom
  })
  
  list(metric = "macro_f1", value = mean(f1_scores, na.rm = TRUE))
}

# Initialize Search
dtrain_smote_nodk <- xgb.DMatrix(data = as.matrix(X_smote_nodk), label = y_smote_num_nodk)

best_f1 <- -Inf
best_params <- list()
best_model <- NULL
best_nrounds <- NULL

# Run Grid Search
for (i in 1:nrow(grid)) {
  cat("\n🔁 Running grid config", i, "of", nrow(grid), "...\n")
  
  params <- list(
    booster = "gbtree",
    objective = "multi:softprob",
    num_class = length(unique(y_smote_num_nodk)),
    max_depth = grid$max_depth[i],
    eta = grid$eta[i],
    min_child_weight = grid$min_child_weight[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = dtrain_smote_nodk,
    nrounds = 200,
    nfold = 5,
    early_stopping_rounds = 10,
    feval = macro_f1_eval,
    maximize = TRUE,
    verbose = 0
  )
  
  best_macro_f1 <- max(cv$evaluation_log$test_macro_f1_mean, na.rm = TRUE)
  best_iter <- which.max(cv$evaluation_log$test_macro_f1_mean)
  
  cat("Best Macro F1:", round(best_macro_f1, 4), "at iteration", best_iter, "\n")
  
  if (best_macro_f1 > best_f1) {
    best_f1 <- best_macro_f1
    best_params <- params
    best_model <- cv
    best_nrounds <- best_iter
  }
}

# train final model with best hyperparamaeters
final_model_smote_nodk <- xgb.train(
  params = best_params,
  data = dtrain_smote_nodk,
  nrounds = best_nrounds,
  verbose = 1
)

# And now evaluate 
val_probs <- predict(final_model_smote_nodk, newdata = dval_smote_nodk)
val_preds <- max.col(matrix(val_probs, ncol = best_params$num_class, byrow = TRUE)) - 1

conf_mat_val <- confusionMatrix(
  factor(val_preds, levels = 0:(best_params$num_class - 1)),
  factor(y_val_num_nodk, levels = 0:(best_params$num_class - 1))
)

f1_smote_grid_nodk <- mean(conf_mat_val$byClass[, "F1"], na.rm = TRUE)
cat("Final Macro F1 (Grid-Optimized SMOTE):", round(f1_smote_grid_nodk, 3), "\n")
print(round(conf_mat_val$byClass[, "F1"], 3))

#### SMOTE in CV #####
# Custom macro F1 evaluation function
macro_f1_eval <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  num_class <- length(unique(labels))
  pred_matrix <- matrix(preds, ncol = num_class, byrow = TRUE)
  pred_labels <- max.col(pred_matrix) - 1
  
  f1_scores <- sapply(0:(num_class - 1), function(class) {
    tp <- sum(pred_labels == class & labels == class)
    fp <- sum(pred_labels == class & labels != class)
    fn <- sum(pred_labels != class & labels == class)
    denom <- (2 * tp + fp + fn)
    if (denom == 0) return(NA)
    2 * tp / denom
  })
  list(metric = "macro_f1", value = mean(f1_scores, na.rm = TRUE))
}

# Grid setup
grid <- expand.grid(
  max_depth = c(3, 4, 5),
  eta = c(0.05, 0.1),
  min_child_weight = c(1, 3),
  subsample = 0.8,
  colsample_bytree = 0.8
)

# 5-fold cross-validation
folds <- createFolds(y_train_num_nodk, k = 5)

best_f1 <- -Inf
best_params <- list()

for (i in 1:nrow(grid)) {
  cat("\n🔁 Grid Config", i, "of", nrow(grid), "\n")
  params <- list(
    booster = "gbtree",
    objective = "multi:softprob",
    num_class = length(unique(y_train_num_nodk)),
    max_depth = grid$max_depth[i],
    eta = grid$eta[i],
    min_child_weight = grid$min_child_weight[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i]
  )
  
  fold_f1s <- c()
  
  for (fold in seq_along(folds)) {
    val_idx <- folds[[fold]]
    train_idx <- setdiff(seq_along(y_train_num_nodk), val_idx)
    
    X_train_fold <- X_train_nodk[train_idx, ]
    y_train_fold <- y_train_num_nodk[train_idx]
    X_val_fold <- X_train_nodk[val_idx, ]
    y_val_fold <- y_train_num_nodk[val_idx]
    
    # One-hot encode
    X_train_ohe <- model.matrix(~ . - 1, X_train_fold)
    X_val_ohe <- model.matrix(~ . - 1, X_val_fold)
    
    # SMOTE within fold
    smote_res <- SMOTE(as.data.frame(X_train_ohe), target = as.factor(y_train_fold), K = 5)
    X_sm <- smote_res$data[, !names(smote_res$data) %in% "class"]
    y_sm <- as.integer(as.factor(smote_res$data$class)) - 1
    
    dtrain <- xgb.DMatrix(data = as.matrix(X_sm), label = y_sm)
    dval <- xgb.DMatrix(data = X_val_ohe, label = y_val_fold)
    
    model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 200,
      early_stopping_rounds = 10,
      watchlist = list(train = dtrain, eval = dval),
      feval = macro_f1_eval,
      maximize = TRUE,
      verbose = 0
    )
    
    preds <- predict(model, newdata = dval)
    pred_labels <- max.col(matrix(preds, ncol = params$num_class, byrow = TRUE)) - 1
    
    f1s <- sapply(0:(params$num_class - 1), function(class) {
      tp <- sum(pred_labels == class & y_val_fold == class)
      fp <- sum(pred_labels == class & y_val_fold != class)
      fn <- sum(pred_labels != class & y_val_fold == class)
      denom <- (2 * tp + fp + fn)
      if (denom == 0) return(NA)
      2 * tp / denom
    })
    
    fold_f1s <- c(fold_f1s, mean(f1s, na.rm = TRUE))
  }
  
  avg_f1 <- mean(fold_f1s, na.rm = TRUE)
  cat("Avg Macro F1:", round(avg_f1, 4), "\n")
  
  if (avg_f1 > best_f1) {
    best_f1 <- avg_f1
    best_params <- params
  }
}

cat("\n Best Macro F1:", round(best_f1, 4), "\n")

# Prepare full training set (with one-hot encoding)
X_train_ohe_nodk <- model.matrix(~ . - 1, data = X_train_nodk)
train_smote_input <- as.data.frame(X_train_ohe_nodk)
train_labels <- as.factor(y_train_num_nodk)

# Apply SMOTE on full training set
smote_result_final <- SMOTE(
  X = train_smote_input,
  target = train_labels,
  K = 5,
  dup_size = 0
)

X_smote_final <- smote_result_final$data[, !names(smote_result_final$data) %in% "class"]
y_smote_final <- as.integer(as.factor(smote_result_final$data$class)) - 1

# Final training matrix
dtrain_final <- xgb.DMatrix(data = as.matrix(X_smote_final), label = y_smote_final)

# Prepare validation matrix
X_val_mat_final <- prepare_validation_matrix(X_val_nodk, colnames(X_smote_final))
dval_final <- xgb.DMatrix(data = X_val_mat_final, label = y_val_num_nodk)

# Train final model with best params
final_model <- xgb.train(
  params = best_params,
  data = dtrain_final,
  nrounds = best_nrounds,
  verbose = 1
)

# Predict and evaluate
val_probs <- predict(final_model, newdata = dval_final)
val_preds <- max.col(matrix(val_probs, ncol = best_params$num_class, byrow = TRUE)) - 1

conf_mat_final <- confusionMatrix(
  factor(val_preds, levels = 0:(best_params$num_class - 1)),
  factor(y_val_num_nodk, levels = 0:(best_params$num_class - 1))
)

# Macro and per-class F1
f1_smote_cv <- mean(conf_mat_final$byClass[, "F1"], na.rm = TRUE)
f1_by_class_final <- conf_mat_final$byClass[, "F1"]

# Print results
cat("Final Macro F1 (Best Params + SMOTE):", round(f1_smote_cv, 3), "\n")
print(round(f1_by_class_final, 3))

# FEATURE PERFORMANCE
# 1. Get feature names from the DMatrix used in training
feature_names <- colnames(X_smote_final)

# 2. Extract importance from final trained model
importance_matrix <- xgb.importance(
  feature_names = feature_names,
  model = final_model
)

# 3. View top features
print(head(importance_matrix, 10))  # top 10 features

# 4. Optional: Plot the top features
xgb.plot.importance(importance_matrix[1:20, ])  # top 20 features

#### STACKED No. 2 ####
# 1. Get prediction matrices for each model
# -- Model 1: XGBoost + SMOTE
pred1 <- predict(final_model, newdata = dval_final)
mat1 <- matrix(pred1, ncol = best_params$num_class, byrow = TRUE)

# -- Model 2: Plain XGBoost (no SMOTE)
pred2 <- predict(model_xgb_nodk, newdata = dval_nodk)
mat2 <- matrix(pred2, ncol = best_params$num_class, byrow = TRUE)

# 2. Align all matrices to 3-class
mat1 <- mat1[, 1:3]
mat2 <- mat2[, 1:3]

# 3. Average predictions
avg_probs <- ((mat1 + mat2)/2)  # Tune these weights

# 4. Get predicted class
stacked_preds <- max.col(avg_probs) - 1

# 5. Evaluate
conf_mat_stacked <- confusionMatrix(
  factor(stacked_preds, levels = 0:(best_params$num_class - 1)),
  factor(y_val_num_nodk, levels = 0:(best_params$num_class - 1))
)

# 6. Output
f1_stacked <- mean(conf_mat_stacked$byClass[, "F1"], na.rm = TRUE)
cat("Macro F1 (Stacked):", round(f1_stacked, 3), "\n")
print(round(conf_mat_stacked$byClass[, "F1"], 3))

#### LIGHT GBM ####

# 1. Clean feature names
colnames(X_smote) <- make.names(colnames(X_smote), unique = TRUE)

# 2. Dataset
dtrain <- lgb.Dataset(data = as.matrix(X_smote), label = y_smote)

# 3. Define parameters
params <- list(
  objective = "multiclass",
  num_class = length(unique(y_smote)),
  learning_rate = 0.1,
  max_depth = 4,
  num_leaves = 31,
  verbosity = -1
)

# 4. Define custom macro F1 eval
macro_f1_eval <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  num_class <- length(unique(labels))
  pred_matrix <- matrix(preds, ncol = num_class, byrow = TRUE)
  pred_labels <- max.col(pred_matrix) - 1
  
  f1s <- sapply(0:(num_class - 1), function(class) {
    tp <- sum(pred_labels == class & labels == class)
    fp <- sum(pred_labels == class & labels != class)
    fn <- sum(pred_labels != class & labels == class)
    denom <- (2 * tp + fp + fn)
    if (denom == 0) return(NA)
    2 * tp / denom
  })
  
  return(list(name = "macro_f1", value = mean(f1s, na.rm = TRUE), higher_better = TRUE))
}

# 5. Run CV — IMPORTANT: disable metric completely via `eval = macro_f1_eval`
set.seed(123)
cv_result <- lgb.cv(
  params = params,
  data = dtrain,
  nrounds = 200,
  nfold = 5,
  early_stopping_rounds = 10,
  eval = "multi_logloss",
  verbose = 1,
  eval_freq = 10
)


best_iter <- cv_result$best_iter
cat("Best iteration from CV:", best_iter, "\n")

# 7. Train final model
final_model_lgb <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = best_iter,
  eval = macro_f1_eval,
  verbose = 1
)

# 8. Predict on validation set
dval <- as.matrix(X_val_ohe)
val_probs <- predict(final_model_lgb, dval)
val_preds <- max.col(matrix(val_probs, ncol = params$num_class, byrow = TRUE)) - 1

# 9. Evaluate
conf_mat <- confusionMatrix(
  factor(val_preds, levels = 0:(params$num_class - 1)),
  factor(y_val_num_nodk, levels = 0:(params$num_class - 1))
)

macro_f1_val <- mean(conf_mat$byClass[, "F1"], na.rm = TRUE)
cat("Macro F1 (LightGBM):", round(macro_f1_val, 3), "\n")
print(round(conf_mat$byClass[, "F1"], 3))

#### SHAP #####

install.packages("treeshap")
library(treeshap)

# Your existing model and data
X_train_mat <- data.matrix(X_train)
dtrain <- xgb.DMatrix(data = X_train_mat, label = y_train_num)

params <- list(
  objective = "multi:softprob",
  num_class = length(unique(y_train_num)),
  eval_metric = "mlogloss"
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100
)

# Convert to treeshap format
xgb_treeshap <- xgboost::xgb.dump(xgb_model, with_stats = TRUE)
xgb_wrapper <- xgboost::xgb.model.dt.tree(model = xgb_model)
unified_model <- treeshap::xgboost.unify(xgb_model, X_train_mat)

shap_result <- treeshap::treeshap(unified_model, X_train_mat, verbose = TRUE)
shap_means <- data.frame(
  feature = colnames(X_train_mat),
  mean_abs_shap = colMeans(abs(shap_result$shaps))
)
# Pick an observation (e.g., row 42)
row_num <- 100

# Get SHAP values and base value for the row
shap_row <- shap_result$shaps[row_num, ]
features_row <- X_train[row_num, ]
base_value <- shap_result$intercept

# Create data frame for plotting
df <- tibble(
  feature = colnames(X_train),
  shap_value = as.numeric(shap_row),
  feature_value = as.character(unlist(features_row))
) %>%
  mutate(abs_shap = abs(shap_value)) %>%
  arrange(desc(abs_shap)) %>%
  head(15)

# Apply readable labels
df$feature_label <- ifelse(
  df$feature %in% names(feature_labels),
  feature_labels[df$feature],
  df$feature  # fallback if no label exists
)

# Create the labeled SHAP plot
shap_plot_readable <- ggplot(df, aes(x = reorder(feature_label, shap_value), y = shap_value, fill = shap_value > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
  labs(
    title = paste("Top SHAP Contributions for Row", row_num),
    x = "Feature",
    y = "SHAP Value (Contribution to Log-Odds)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

# Save it to file
ggsave(
  filename = "shap_row100_readable.png",
  plot = shap_plot_readable,
  width = 12,
  height = 8,
  dpi = 300
)

## Trying Again
# Create a lookup table for feature labels
feature_lookup <- tibble(
  feature = names(feature_labels),
  feature_label = unname(feature_labels)
)

# Create data frame for plotting
df <- tibble(
  feature = colnames(X_train),
  shap_value = as.numeric(shap_row),
  feature_value = as.character(unlist(features_row))
) %>%
  mutate(abs_shap = abs(shap_value)) %>%
  arrange(desc(abs_shap)) %>%
  head(15) %>%
  left_join(feature_lookup, by = "feature") %>%
  mutate(feature_label = ifelse(is.na(feature_label), feature, feature_label))

shap_plot_readable <- ggplot(df, aes(x = reorder(feature_label, shap_value), y = shap_value, fill = shap_value > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
  labs(
    title = paste("Top SHAP Contributions for Row", row_num),
    x = "Feature",
    y = "SHAP Value (Contribution to Log-Odds)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  filename = "shap_row100_readable.png",
  plot = shap_plot_readable,
  width = 12,
  height = 8,
  dpi = 300
)


# Create summary of mean absolute SHAP values
shap_means <- data.frame(
  feature = colnames(X_train),
  mean_abs_shap = colMeans(abs(shap_result$shaps))
)

# Select top 20 features
top_shap <- shap_means %>%
  arrange(desc(mean_abs_shap)) %>%
  head(20)

feature_labels <- c(
  s01_5 = "Like/dislike scale: Green Party",
  l09 = "Government obligation to income equality",
  Age = "Age",
  p03_1 = "Personal views of EU integration",
  g01_6 = "Opinions on Green Party tax policy",
  r03 = "Opinions on women's equality movements",
  g01_2 = "Opinions on Conservative Party tax policy",
  n03 = "Trust in politicians",
  f01_10 = "Increase tolerance of unconventional lifestyles",
  i01_3 = " Like/dislike leader: Swinson",
  p03_2 = "Conservative Party views on EU integration",
  e01 = "Left/Right scale",
  g01_1 = "Personal opinions on tax policy",
  m02_1 = "Agree/Disagree: Politicians don’t care what people like me think",
  x3 = "Party",
  u01 = "Frequency of political conversations",
  mii_coded = "Most important issue: Environment",
  p03a = "Importance of EU membership vs. immigration",
  y01 = "Annual income",
  i01_6 = "Like/dislike leader: Farage",
  g01_5 = "Opinion on Lib/Dem tax policy",
  f01_1 = "Agree/Disagree: Ordinary working people get their fair share",
  t01_2 = "Shout govt. spend less on foreign aid?",
  w03 = "Growing up were you middle class or working class?",
  i01_5 = "Like/dislike leader: Bartley"
)

label_info$x3

shap_means$feature_readable <- feature_labels[shap_means$feature]

# Plot using readable names
full_shap <- ggplot(shap_means %>% slice_max(mean_abs_shap, n = 10), aes(x = reorder(feature_readable, mean_abs_shap), y = mean_abs_shap)) +
  geom_col(fill = "gray") +
  coord_flip() +
  labs(
    title = "Figure 3: Top Features by Mean Absolute SHAP Value",
    x = "Feature",
    y = "Mean SHAP",
    caption = "Higher values = more influence on predictions \n Questions have been reworded for readability"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14)
  )

ggsave(
  filename = "shap_mean_plot_readable.png",
  plot = full_shap,
  width = 14,
  height = 6.5,   # slightly taller for better text visibility
  dpi = 300
)

# Define your desired order of responses
ordered_levels <- c(
  "Growth priority",
  "2", "3", "4", "5", "6", "7", "8", "9", "10",
  "Environment priority",
  "Don't know"
)

bes_clean$h01 <- factor(bes_clean$h01, levels = ordered_levels, ordered = TRUE)

# Distribution of H01 responses
ggplot(bes_clean, aes(x = h01)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Figure 1: Distribution of H01 Responses",
    x = "H01: Environment vs. Economy Preference",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

ggsave("h01_distribution2.png", width = 8, height = 6, dpi = 300)

# Create bar plot
ggplot(data.frame(Response = y_grouped), aes(x = Response)) +
  geom_bar(fill = "#377eb8") +
  labs(
    title = "Figure 2: Distribution of Grouped H01 Responses",
    x = "Grouped H01 Response",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

ggsave("h01_grouped_distribution2.png", width = 8, height = 6, dpi = 300)

#### ON TEST DATA ####

##### No DK + SMOTE: Test Set Evaluation #####

# One-hot encode test features to match SMOTE train structure
X_test_mat_nodk <- prepare_validation_matrix(X_test_nodk, colnames(X_smote_nodk))
dtest_smote_nodk <- xgb.DMatrix(data = X_test_mat_nodk, label = y_test_num_nodk)

# Generate predictions on test set
pred_matrix_test_smote_nodk <- matrix(
  predict(model_smote_nodk, newdata = dtest_smote_nodk),
  ncol = params_smote_nodk$num_class,
  byrow = TRUE
)

pred_classes_test_smote_nodk <- max.col(pred_matrix_test_smote_nodk) - 1

# Confusion matrix on test data
conf_mat_test_smote_nodk <- confusionMatrix(
  factor(pred_classes_test_smote_nodk, levels = 0:(params_smote_nodk$num_class - 1)),
  factor(y_test_num_nodk, levels = 0:(params_smote_nodk$num_class - 1))
)

# Create F1 score table for test set
f1_class_test <- round(conf_mat_test_smote_nodk$byClass[, "F1"], 3)
f1_macro_test <- round(f1_test_smote_nodk, 3)

f1_df_test <- data.frame(
  Class = c("Growth priority", "No strong preference", "Environment priority", "Macro Avg"),
  `F1 Score` = c(f1_class_test, f1_macro_test)
)

# Display in markdown slides
kable(f1_df_test, caption = "F1 Scores by Class and Macro Average (SMOTE model, No 'Don't Know', Test Set)")

# Save test set confusion matrix
saveRDS(conf_mat_test_smote_nodk, "conf_mat_test_smote_nodk2.rds")

