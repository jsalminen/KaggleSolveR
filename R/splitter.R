# Splitter
# - Input: train data, target col, task type
# - Output: train_X, valid_X, train_Y, valid_Y
# - Classification: stratified splitting
# - Regression: K-fold split

#' Split data to training and validation sets
#' @export
#' @param df A data frame
#' @param target_col A string
#' @return A list of data frames
splitData <- function(df, target_col) {
    # Get target and validation set indexes
    index <- getPartitionIndex(df[[target_col]])

    # Split data table to train and validation sets
    train_x <- df[index, ]
    valid_x <- df[-index, ]

    # Extract target columns
    train_y <- train_x[[target_col]]
    valid_y <- valid_x[[target_col]]

    # Remove target columns from features
    train_x[, target_col] <- NULL
    valid_x[, target_col] <- NULL

    # Add datasets to a list and return
    return(list(train_x = train_x,
                train_y = train_y,
                valid_x = valid_x,
                valid_y = valid_y))
}

#' Get partition index for training and validation sets
#' @param target_col A vector
#' @return A vector
getPartitionIndex <- function(target_col) {
    index <- caret::createDataPartition(target_col, list = FALSE)
    return(index)
}

