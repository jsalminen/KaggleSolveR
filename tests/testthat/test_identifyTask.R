context("Identify task")

raw_data <- read.csv2("test_data/splitData_raw.csv")

test_that("task type is identified correctly", {
    expect_equal(identifyTask(raw_data$factor_target), "classification")
    expect_equal(identifyTask(raw_data$numeric_target), "regression")
})
