context("Preprocessing data")

expected_col_classes <- c("numeric",
                          "character",
                          "factor",
                          "factor",
                          "numeric",
                          "character")

test_data <- read.csv2("test_data/preprocess_testdata.csv",
                       colClasses = expected_col_classes)

test_that("numeric preprocessing works", {
    preProcNumeric <- getPreProcNumeric(test_data)
    df <- predict(preProcNumeric, test_data)

    col_classes <- sapply(df, class)
    col_classes <- unname(col_classes)

    expect_equal(col_classes, expected_col_classes)

    expect_equal(mean(df$numeric_1), 0)
    expect_equal(sd(df$numeric_1), 1)
    expect_equal(mean(df$numeric_2), 0)
    expect_equal(sd(df$numeric_2), 1)
})
