context("Clean data")

col_classes <- c("factor",
                 "factor",
                 "numeric",
                 "numeric",
                 "character",
                 "character",
                 "numeric",
                 "numeric",
                 "numeric",
                 "character",
                 "character",
                 "factor",
                 "factor",
                 "numeric",
                 "numeric",
                 "Date",
                 "Date",
                 "factor",
                 "factor")

expected_col_names <- c("factor_to_numeric",
                        "factor_to_character",
                        "numeric_to_factor",
                        "numeric_to_character",
                        "character_to_factor",
                        "character_to_numeric",
                        # should be gone "drop_column",
                        "id_column",
                        "target_column",
                        "impute_char_1",
                        "impute_char_2",
                        "impute_factor_1",
                        "impute_factor_2",
                        "impute_numeric_1",
                        "impute_numeric_2",
                        "impute_date_1",
                        "impute_date_2",
                        "new_train_factor",
                        "new_test_factor")

train <- read.csv2("cleanData_train.csv", colClasses = col_classes)
test <- read.csv2("cleanData_test.csv", colClasses = col_classes)
config_file <- read.csv2("config_file.csv", stringsAsFactors = FALSE)

cleaned_data <- cleanData(train, test, config_file)
train <- cleaned_data$train
test <- cleaned_data$test

test_that("correct columns are dropped", {
    expect_equal(names(train), expected_col_names)
    expect_equal(names(test), expected_col_names)
})

test_that("classes are updated correctly", {
    expect_equal(class(train$factor_to_numeric), "numeric")
    expect_equal(class(train$factor_to_character), "character")
    expect_equal(class(train$numeric_to_factor), "factor")
    expect_equal(class(train$numeric_to_character), "character")
    expect_equal(class(train$character_to_factor), "factor")
    expect_equal(class(train$character_to_numeric), "numeric")

    expect_equal(class(test$factor_to_numeric), "numeric")
    expect_equal(class(test$factor_to_character), "character")
    expect_equal(class(test$numeric_to_factor), "factor")
    expect_equal(class(test$numeric_to_character), "character")
    expect_equal(class(test$character_to_factor), "factor")
    expect_equal(class(test$character_to_numeric), "numeric")
})

test_that("NAs are imputed correctly", {
    expect_equal(train$impute_char_1[2], "imputed text")
    expect_equal(train$impute_char_2[2], "NA-value")

    expect_equal(class(train$impute_factor_1), "factor")
    expect_equal(levels(train$impute_factor_1), c("level1", "level2", "new level"))
    expect_equal(as.character(train$impute_factor_1[2]), "new level")
    expect_equal(class(train$impute_factor_2), "factor")
    expect_equal(levels(train$impute_factor_2), c("level2", "level3", "NA-value"))
    expect_equal(as.character(train$impute_factor_2[2]), "NA-value")

    expect_equal(train$impute_numeric_1[2], 7)
    expect_equal(train$impute_numeric_2[2], 0)

    expect_equal(train$impute_date_1[2], as.Date("2017-01-01"))
    expect_equal(is.na(train$impute_date_2[2]), TRUE)
})

test_that("actor levels are fixed correctly", {
    expect_equal(levels(train$new_train_factor), levels(test$new_train_factor))
    expect_equal(levels(train$new_test_factor), levels(test$new_test_factor))
})



