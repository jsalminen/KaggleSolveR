

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

    train <- update_classes(train, config_file)
    train <- imputeNAs(train, config_file)

    test <- update_classes(test, config_file)
    test <- imputeNAs(test, config_file)

    data <- fix_factor_levels(train, test)
    train <- data$train
    test <- data$test

    train <- train %>%
        dplyr::select(-dplyr::one_of(drop_cols))
    test <- test %>%
        dplyr::select(-dplyr::one_of(drop_cols))

    return(list(train = train,
                test = test))
}

#' Create configuration file for a data set
#' @export
#' @param df A data frame containing the training data set
#' @return A data frame
createConfigFile <- function(df) {
    column_names <- names(df)
    column_classes <- sapply(df, class)
    NA_counts <- countNAs(df)

    df <- data.frame("name" = column_names,
                     "class" = column_classes,
                     "new_class" = column_classes,
                     "NA_count" = NA_counts,
                     "impute" = "",
                     "impute_value" = "",
                     "id_column" = "",
                     "target_column" = "",
                     "drop_column" = "",
                     stringsAsFactors = FALSE)

    return(df)
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

    for (n in config_file$name[config_file$new_class == "character"]) {
        if (n %in% names(data)) {
            data[, n] <- as.character(data[, n])
        }
    }

    return(data)
}

#' Synchronize factor levels between train and test sets
#' @param train A data frame containing the training data
#' @param test A data frame containing the test data
#' @return A list containing train and test data frames
fix_factor_levels <- function(train, test) {
    # Get factor column names
    factor_cols <- names(train)[sapply(train, is.factor)]

    # Process only columns that are found also in test set
    factor_cols <- factor_cols[factor_cols %in% names(test)]

    for (col in factor_cols) {
        if (!isTRUE(all.equal(levels(train[, col]), levels(test[, col])))) {
            new_levels <- c(levels(train[, col]), "new_level")
            train[, col] <- factor(train[, col], levels = new_levels)

            levels(test[, col]) <- c(levels(test[, col]), "new_level")
            test[!(test[, col] %in% new_levels), col] <- "new_level"
            test[, col] <- factor(test[, col], levels = new_levels)
        }
    }

    return(list(train = train, test = test))
}

addMissingFactorLevels <- function(x, new_levels) {
    if(!(class(x) == "factor")) return(x)

    current_levels <- levels(x)

    for (level in new_levels) {
        if (!(level %in% current_levels)) {
            addFactorLevel(x, level)
        }
    }

    return(x)
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

    if (class(x) == "numeric") {
        impute_value <- as.numeric(impute_value)
    }

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






