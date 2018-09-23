

#' Clean training and test data sets
#' @export
#' @param train A data frame containing the training data
#' @param test A data frame containing the test data
#' @param config_file A data frame containing the configuration for the data set
#' @return A list containing cleaned train and test data frames
cleanData <- function(train, test, config_file) {
    `%>%` <- magrittr::`%>%`

    drop_cols <- config_file %>%
        dplyr::filter(drop_column == 1) %>%
        dplyr::pull(name)

    train <- train %>%
        dplyr::select(-dplyr::one_of(drop_cols))
    train <- update_classes(train, config_file)
    train <- imputeNAs(train, config_file)

    test <- test %>%
        dplyr::select(-dplyr::one_of(drop_cols))
    test <- update_classes(test, config_file)
    test <- imputeNAs(test, config_file)

    data <- fix_factor_levels(train, test)
    train <- data$train
    test <- data$test

    return(list(train = train,
                test = test))
}

#' Create configuration file for a data set
#' @export
#' @param data A data frame containing the training data set
#' @param path A string for the path where the configuration file will be saved
#' @param overwrite A boolean specifying if an existing file is written over
#' @return A string
createConfigFile <- function(data, path = "", overwrite = FALSE) {
    file_name <- paste(path, "column_config.csv", sep = "")

    if (file.exists(file_name) & !overwrite) {
        return("Configuration file already exists")
    }

    column_names <- names(data)
    column_classes <- sapply(data, class)
    NA_counts <- countNAs(data)

    df <- data.frame("name" = column_names,
                     "class" = column_classes,
                     "new_class" = column_classes,
                     "NA_count" = NA_counts,
                     "impute" = "",
                     "impute_value" = "",
                     "id_column" = "",
                     "target_column" = "",
                     "drop_column" = "")

    write.csv2(df, file = file_name, row.names = FALSE)

    return(paste("Created configuration file", file_name))
}

#' Count the number of NAs in each column of a data frame
#' @param df A data frame
#' @return A vector
countNAs <- function(df) {
    NA_counts <- vapply(df, function(x) sum(is.na(x)), FUN.VALUE = numeric(1))
    return(NA_counts)
}

#' Change classes in a data frame according to configuration file
#' @param data A data frame
#' @param config_file A data frame containing the configuration data
#' @return A data frame
update_classes <- function(data, config_file) {
    for (n in config_file$name[config_file$new_class == "factor"]) {
        if (n %in% names(data) & class(data[, n]) != "factor") {
            data[, n] <- as.factor(data[, n])
        }
    }

    for (n in config_file$name[config_file$new_class == "numeric"]) {
        if (n %in% names(data)) {
            data[, n] <- as.numeric(data[, n])
        }
    }

    return(data)
}

#' Synchronize factor levels between train and test sets
#' @param train_x A data frame containing the training data
#' @param test_x A data frame containing the test data
#' @return A list containing train_x and test_x data frames
fix_factor_levels <- function(train_x, test_x) {
    # Get factor column names
    factor_cols <- names(train_x)[sapply(train_x, is.factor)]

    # Process only columns that are found also in test set
    factor_cols <- factor_cols[factor_cols %in% names(test_x)]

    for (col in factor_cols) {
        if (!isTRUE(all.equal(levels(train_x[, col]), levels(test_x[, col])))) {
            train_x[, col] <- addFactorLevel(train_x[, col], "new_test_level")
            new_levels <- levels(train_x[, col])

            test_x[, col] <- addFactorLevel(test_x[, col], "new_test_level")
            test_x[!test_x[, col] %in% new_levels, col] <- "new_test_level"
            levels(test_x[, col]) <- new_levels
        }
    }

    return(list(train = train_x, test = test_x))
}

################################################################################

#' Impute missing values in train or test data set
#' @param df A data frame
#' @param config_file A data frame containing the configuration data
#' @return A data frame
imputeNAs <- function(df, config_file) {
    `%>%` <- magrittr::`%>%`

    # Get columns with more than 0 NAs
    impute_info <- config_file %>%
        dplyr::filter(NA_count > 0)

    for (i in 1:nrow(impute_info)) {
        column <- impute_info[i, "name"]
        impute <- impute_info[i, "impute"]
        impute_value <- impute_info[i, "impute_value"]

        df[, column] <- imputeNA(df[, column], impute, impute_value)
    }

    return(df)
}

#' Impute missing values in a column or vector
#' @param x A vector
#' @param impute A string specifying imputation method
#' @param impute_value Constant value for replacing NAs
#' @return A vector
imputeNA <- function(x, impute = NA, impute_value = NULL) {

    if (is.na(impute)) {
        x <- imputeDefault(x)
    }else if (impute == "value") {
        x <- imputeValue(x, impute_value)
    } else  {
        x <- imputeDefault(x)
    }

    return(x)
}

#' Replace NAs with a specific value
#' @param x A vector
#' @param impute_value A value for replacing NAs
#' @return A vector
imputeValue <- function(x, impute_value) {

    if (class(x) == "factor") {
        x <- addFactorLevel(x, impute_value)
    }

    x[is.na(x)] <- impute_value

    return(x)
}

#' Add a new level to a factor
#' @param x A factor
#' @param new_level A string for new factor level
#' @return A factor
addFactorLevel <- function(x, new_level) {

    if (class(x) != "factor") {return(x)}

    new_levels <- levels(x)
    if (!(new_level %in% new_levels)) {
        new_levels <- c(new_levels, new_level)
    }

    levels(x) <- new_levels

    return(x)
}

#' Impute missing values with default values
#' @param x A factor, character, or numeric vector
#' @return A vector
imputeDefault <- function(x) {
    if (is.numeric(x)) {
        x <- imputeValue(x, 0)
    } else if (is.factor(x) || is.character(x)) {
        x <- imputeValue(x, "NA-value")
    }
    return(x)
}


################################################################################

#' Create new features based on date fields
#' @param df A data frame
#' @return A data frame
createDateFeatures <- function(df) {
    date_cols <- names(df)[sapply(df, function(df) class(df) == "Date")]

    for (col in date_cols) {
        df[, paste(col, "_year", sep = "")] <- lubridate::year(df[, col])
        df[, paste(col, "_month", sep = "")] <- lubridate::month(df[, col])
        df[, paste(col, "_day", sep = "")] <- lubridate::day(df[, col])
        df[, paste(col, "_weekday", sep = "")] <- lubridate::wday(df[, col])
    }

    return(df)
}






