
createConfigFile_for_testing <- function() {
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

    train <- read.csv2("cleanData_train.csv",
                       colClasses = col_classes)

    config_file <- createConfigFile(train)
    write.csv2(config_file, "test_data/config_file_template.csv", row.names = FALSE)
}

createTestDataForSplitting <- function() {
    row_count <- 100

    id <- 1:row_count

    set.seed(74839)
    feature_1 <- rnorm(row_count)
    feature_2 <- rnorm(row_count)

    factor_target_base <- feature_1 * feature_2
    factor_target <- rep("60 percent", row_count)
    factor_target[factor_target_base < quantile(factor_target_base,
                                                   probs = c(0.1))] <- "10 percent"
    factor_target[factor_target_base > quantile(factor_target_base,
                                                   probs = c(0.70))] <- "30 percent"
    factor_target <- as.factor(factor_target)
    numeric_target <- feature_1 * (feature_2 + 0.2)

    df <- data.frame(id, feature_1, feature_2, factor_target, numeric_target)
    write.csv2(df, "test_data/splitData_raw.csv", row.names = FALSE)
}

createTitanicConfigFile <- function(replace_existing = FALSE) {
    filename <- "test_data/titanic_config_file.csv"

    train <- read.csv("test_data/titanic_train.csv")

    config_file <- createConfigFile(train)

    if (replace_existing | !file.exists(filename)) {
        write.csv2(config_file, filename, row.names = FALSE)
    }
}

createHousingConfigFile <- function(replace_existing = FALSE) {
    filename <- "test_data/housing_config_file.csv"

    train <- read.csv("test_data/housing_train.csv")

    config_file <- createConfigFile(train)

    if (replace_existing | !file.exists(filename)) {
        write.csv2(config_file, filename, row.names = FALSE)
    }
}

createPreprocessTestData <- function() {
    filename <- "test_data/preprocess_testdata.csv"

    row_count <- 100

    numeric_1 <- rnorm(row_count, mean = 1000, sd = 50)
    character_1 <- rep("text", row_count)
    factor_1 <- rep(c("level1", "level2"), row_count / 2)
    numeric_2 <- rnorm(row_count, mean = -1000, sd = 50)
    character_2 <- rep("text2", row_count)
    factor_2 <- rep(c("level3", "level4"), row_count / 2)

    df <- data.frame(numeric_1,
                     character_1,
                     factor_1,
                     factor_2,
                     numeric_2,
                     character_2)

    write.csv2(df, filename, row.names = FALSE)
}
