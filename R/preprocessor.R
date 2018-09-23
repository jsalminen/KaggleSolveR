

#' Preprocess all numeric columns of a data frame
#' @param df A data frame
#' @return A function
getPreProcNumeric <- function(df) {
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    preProc <- caret::preProcess(df, method = list(center = numeric_cols,
                                                   scale = numeric_cols))
    return(preProc)
}

#' Preprocess all factor columns of a data frame
#' @param df A data frame
#' @return A data frame
getPreProcFactor <- function(dt) {
    return(dt)
}

#' Preprocess all character columns of a data frame
#' @param df A data frame
#' @return A data frame
processText <- function(dt) {
    return(dt)
}

