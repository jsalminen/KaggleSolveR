context("Create configuration file")

expected_col_names <- c("name",
                        "class",
                        "new_class",
                        "NA_count",
                        "impute",
                        "impute_value",
                        "id_column",
                        "target_column",
                        "drop_column")

col_classes <- c("character",
                 "numeric",
                 "factor",
                 "logical",
                 "character")

expected_na_counts <- c(2, 0, 1, 0, 5)

test_that("configuration file is created correctly", {
    df <- read.csv2("createConfigFile_testdata.csv",
                    colClasses = col_classes)

    config_file <- createConfigFile(df)

    expected_names <- names(df)
    column_names <- names(config_file)

    expect_equal(expected_col_names, column_names)
    expect_equal(expected_names, config_file$name)
    expect_equal(col_classes, config_file$class)
    expect_equal(col_classes, config_file$new_class)
    expect_equal(expected_na_counts, config_file$NA_count)
})
