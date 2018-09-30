context("Select model")

best_fit_model_names <- c("best_model_name",
                          "best_score",
                          "best_score_train",
                          "best_model")


test_that("model selector works on factor target", {

    train <- read.csv("test_data/titanic_train.csv")
    test <- read.csv("test_data/titanic_test.csv")

    # Read config file
    config_file <- read.csv2("test_data/titanic_config_file.csv",
                             stringsAsFactors = FALSE)

    # Clean data
    cleaned_data <- cleanData(train, test, config_file)
    train <- cleaned_data$train
    test <- cleaned_data$test

    rm(cleaned_data)

    # Split data
    new_data <- splitData(train, "Survived")

    train_x <- new_data$train_x
    train_y <- new_data$train_y
    valid_x <- new_data$valid_x
    valid_y <- new_data$valid_y

    rm(new_data)

    # Identify task
    task_type <- identifyTask(train_y)

    # Preprocess data
    preProcNumeric <- getPreProcNumeric(train_x)
    train_x <- predict(preProcNumeric, train_x)
    valid_x <- predict(preProcNumeric, valid_x)

    # Select model
    best_fit_model <- selectModel(train_x, train_y,
                                  valid_x, valid_y,
                                  task_type, "accuracy",
                                  minimize_score = FALSE,
                                  verbose = FALSE)

    expect_equal(names(best_fit_model), best_fit_model_names)
    expect_gt(best_fit_model$best_score, 0.79)

})

test_that("model selector works on numeric target", {
    train <- read.csv("test_data/housing_train.csv")
    test <- read.csv("test_data/housing_test.csv")

    # Read config file
    config_file <- read.csv2("test_data/housing_config_file.csv",
                             stringsAsFactors = FALSE)

    # Clean data
    cleaned_data <- cleanData(train, test, config_file)
    train <- cleaned_data$train
    test <- cleaned_data$test

    rm(cleaned_data)

    # Split data
    new_data <- splitData(train, "SalePrice")

    train_x <- new_data$train_x
    train_y <- new_data$train_y
    valid_x <- new_data$valid_x
    valid_y <- new_data$valid_y

    rm(new_data)

    # Identify task
    task_type <- identifyTask(train_y)

    # Preprocess data
    preProcNumeric <- getPreProcNumeric(train_x)
    train_x <- predict(preProcNumeric, train_x)
    valid_x <- predict(preProcNumeric, valid_x)

    # Select model
    best_fit_model <- selectModel(train_x, train_y,
                                  valid_x, valid_y,
                                  task_type, "rmse",
                                  best_fit_model = NULL,
                                  verbose = FALSE)

    expect_equal(names(best_fit_model), best_fit_model_names)
    expect_lt(best_fit_model$best_score, 35000)
})
