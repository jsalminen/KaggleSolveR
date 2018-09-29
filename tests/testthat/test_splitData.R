context("Split data")



raw_data <- read.csv2("test_data/splitData_raw.csv")

test_that("factor target is split correctly", {
    split_data <- splitData(raw_data, "factor_target")
    train_x <- split_data$train_x
    train_y <- split_data$train_y
    valid_x <- split_data$valid_x
    valid_y <- split_data$valid_y

    feature_names <- c("id", "feature_1", "feature_2", "numeric_target")

    expect_equal(names(train_x), feature_names)
    expect_equal(names(valid_x), feature_names)

    expect_equal(length(train_y), 50)
    expect_equal(length(train_y[train_y == "60 percent"]), 30)
    expect_equal(length(train_y[train_y == "30 percent"]), 15)
    expect_equal(length(train_y[train_y == "10 percent"]), 5)

    expect_equal(length(valid_y), 50)
    expect_equal(length(valid_y[valid_y == "60 percent"]), 30)
    expect_equal(length(valid_y[valid_y == "30 percent"]), 15)
    expect_equal(length(valid_y[valid_y == "10 percent"]), 5)
})

test_that("numeric target is split correctly", {
    split_data <- splitData(raw_data, "numeric_target", p = 0.8)
    train_x <- split_data$train_x
    train_y <- split_data$train_y
    valid_x <- split_data$valid_x
    valid_y <- split_data$valid_y

    feature_names <- c("id", "feature_1", "feature_2", "factor_target")

    expect_equal(names(train_x), feature_names)
    expect_equal(names(valid_x), feature_names)

    expect_equal(length(train_y), 80)
    expect_equal(length(valid_y), 20)

})



