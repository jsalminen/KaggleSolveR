context("Select model")

best_fit_model_names <- c("best_model_name",
                          "best_score",
                          "best_score_train",
                          "best_model")

test_that("model selector works on factor target", {

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

    # Select model
    best_fit_model <- suppressWarnings(selectModel(train_x, train_y,
                                  valid_x, valid_y,
                                  task_type, METRIC,
                                  minimize_score = FALSE,
                                  verbose = FALSE))

    expect_equal(names(best_fit_model), best_fit_model_names)
    expect_gt(best_fit_model$best_score, 0.79)

    rm(list = ls())
})


test_that("model selector works on numeric target", {
    `%>%` <- magrittr::`%>%`

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

    # Select model
    best_fit_model <- suppressWarnings(selectModel(train_x, train_y,
                                  valid_x, valid_y,
                                  task_type, METRIC,
                                  best_fit_model = NULL,
                                  verbose = FALSE))

    expect_equal(names(best_fit_model), best_fit_model_names)
    expect_lt(best_fit_model$best_score, 35000)

    rm(list = ls())
})
