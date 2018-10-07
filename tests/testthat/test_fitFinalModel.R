context("Final model")

`%>%` <- magrittr::`%>%`

CONFIG <- "test_data/titanic_config_file.csv"
TRAIN <- "test_data/titanic_train.csv"
TEST <- "test_data/titanic_test.csv"
METRIC <- "accuracy"
TARGET <- "Survived"

# Load data
train <- read.csv(TRAIN)
test <- read.csv(TEST)

# Clean data
config_file <- read.csv2(CONFIG, stringsAsFactors = FALSE)

id_cols <- config_file %>%
    dplyr::filter(id_column == 1) %>%
    dplyr::pull(name)

target_cols <- config_file %>%
    dplyr::filter(target_column == 1) %>%
    dplyr::pull(name)

data <- cleanData(train, test, config_file)

train <- data$train
test <- data$test

train_id <- train %>%
    dplyr::select(id_cols)
train <- train %>%
    dplyr::select(-dplyr::one_of(id_cols))

test_id <- test %>%
    dplyr::select(id_cols)
test <- test %>%
    dplyr::select(-dplyr::one_of(id_cols))

rm(data)

## Split data

new_data <- splitData(train, TARGET)

train_x <- new_data$train_x
train_y <- new_data$train_y
valid_x <- new_data$valid_x
valid_y <- new_data$valid_y

rm(new_data)

## Identify task

task_type <- identifyTask(train_y)

## Preprocess data
preProcNumeric <- getPreProcNumeric(train_x)
train_x <- predict(preProcNumeric, train_x)
valid_x <- predict(preProcNumeric, valid_x)

# Full data set
train_y_full <- train %>%
    dplyr::pull(target_cols)
train_full <- train %>%
    dplyr::select(-dplyr::one_of(target_cols))

train_full <- predict(preProcNumeric, newdata = train_full)


# XGBoost
test_that("fitting final model on factor/XGBoost works", {

    best_fit_model <- suppressWarnings(selectModel(train_x, train_y,
                                  valid_x, valid_y,
                                  task_type, METRIC,
                                  minimize_score = FALSE,
                                  verbose = FALSE,
                                  model_list = c("xgboost")))

    final_model <- fitFinalModel(train_full, train_y_full,
                                 best_fit_model, task_type, METRIC)

    expect_lt(final_model$best_score, 0.2)

})

# Random forest
test_that("fitting final model on factor/RandomForest works", {

    best_fit_model <- selectModel(train_x, train_y,
                                  valid_x, valid_y,
                                  task_type, METRIC,
                                  minimize_score = FALSE,
                                  verbose = FALSE,
                                  model_list = c("randomforest"))

    final_model <- fitFinalModel(train_full, train_y_full,
                                 best_fit_model, task_type, METRIC)

    expect_lt(final_model$best_score, 0.2)

})

# Logistic regression
test_that("fitting final model on factor/logistic regression works", {

    best_fit_model <- selectModel(train_x, train_y,
                                  valid_x, valid_y,
                                  task_type, METRIC,
                                  minimize_score = FALSE,
                                  verbose = FALSE,
                                  model_list = c("logisticRegression"))

    final_model <- fitFinalModel(train_full, train_y_full,
                                 best_fit_model, task_type, METRIC)

    expect_lt(final_model$best_score, 0.2)

})


# NUMERIC TARGET

CONFIG <- "test_data/housing_config_file.csv"
TRAIN <- "test_data/housing_train.csv"
TEST <- "test_data/housing_test.csv"
METRIC <- "rmse"
TARGET <- "SalePrice"

# Load data
train <- read.csv(TRAIN)
test <- read.csv(TEST)

# Clean data
config_file <- read.csv2(CONFIG, stringsAsFactors = FALSE)

id_cols <- config_file %>%
    dplyr::filter(id_column == 1) %>%
    dplyr::pull(name)

target_cols <- config_file %>%
    dplyr::filter(target_column == 1) %>%
    dplyr::pull(name)

data <- cleanData(train, test, config_file)

train <- data$train
test <- data$test

train_id <- train %>%
    dplyr::select(id_cols)
train <- train %>%
    dplyr::select(-dplyr::one_of(id_cols))

test_id <- test %>%
    dplyr::select(id_cols)
test <- test %>%
    dplyr::select(-dplyr::one_of(id_cols))

rm(data)

## Split data

new_data <- splitData(train, TARGET)

train_x <- new_data$train_x
train_y <- new_data$train_y
valid_x <- new_data$valid_x
valid_y <- new_data$valid_y

rm(new_data)


## Identify task
task_type <- identifyTask(train_y)

## Preprocess data
preProcNumeric <- getPreProcNumeric(train_x)
train_x <- predict(preProcNumeric, train_x)
valid_x <- predict(preProcNumeric, valid_x)

# Full data set
train_y_full <- train %>%
    dplyr::pull(target_cols)
train_full <- train %>%
    dplyr::select(-dplyr::one_of(target_cols))

train_full <- predict(preProcNumeric, newdata = train_full)

# XGBoost
test_that("fitting final model on numeric/XGBoost works", {

    best_fit_model <- suppressWarnings(selectModel(train_x, train_y,
                                  valid_x, valid_y,
                                  task_type, METRIC,
                                  minimize_score = FALSE,
                                  verbose = FALSE,
                                  model_list = c("xgboost")))

    final_model <- fitFinalModel(train_full, train_y_full,
                                 best_fit_model, task_type, METRIC)

    expect_lt(final_model$best_score, 35000)

})


# Random Forest
test_that("fitting final model on numeric/RandomForest works", {

    best_fit_model <- selectModel(train_x, train_y,
                                  valid_x, valid_y,
                                  task_type, METRIC,
                                  minimize_score = FALSE,
                                  verbose = FALSE,
                                  model_list = c("randomForest"))

    final_model <- fitFinalModel(train_full, train_y_full,
                                 best_fit_model, task_type, METRIC)

    expect_lt(final_model$best_score, 35000)

})

# Lasso
test_that("fitting final model on numeric/Lasso works", {

    best_fit_model <- selectModel(train_x, train_y,
                                  valid_x, valid_y,
                                  task_type, METRIC,
                                  minimize_score = FALSE,
                                  verbose = FALSE,
                                  model_list = c("Lasso"))

    final_model <- fitFinalModel(train_full, train_y_full,
                                 best_fit_model, task_type, METRIC)

    expect_lt(final_model$best_score, 35000)

})


# Linear regression
test_that("fitting final model on numeric/Linear regression works", {

    best_fit_model <- selectModel(train_x, train_y,
                                  valid_x, valid_y,
                                  task_type, METRIC,
                                  minimize_score = FALSE,
                                  verbose = FALSE,
                                  model_list = c("linearRegression"))

    final_model <- fitFinalModel(train_full, train_y_full,
                                 best_fit_model, task_type, METRIC)

    expect_lt(final_model$best_score, 35000)

})
