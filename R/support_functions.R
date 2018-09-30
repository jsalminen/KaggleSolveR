
#' Change the data representation depending to fit requirements of models
#' @param df A data frame
#' @param model_name A string
#' @return A data frame
fixDataRepresentation <- function(df, model_name) {

    model_name <- tolower(model_name)

    if (model_name == "xgboost") {
        df <- getSparseMatrix(df)
    }
    else if (model_name == "lasso") {
        df <- getModelMatrix(df)
    }
    else if (model_name == "linearregression") {
        df <- as.data.frame(getModelMatrix(df))
    }

    return(df)
}

#' Convert a data frame to a sparse model matrix
#' @param df A data frame
#' @return A sparse model matrix
getSparseMatrix <- function(df) {
    previous_na_action <- options('na.action')
    options(na.action='na.pass')

    df_sparse <- Matrix::sparse.model.matrix(~ .-1, data = df,
                                                  drop.unused.levels = FALSE)
    options(na.action=previous_na_action$na.action)

    return(df_sparse)
}

#' Convert a data frame to a model matrix
#' @param df A data frame
#' @return A model matrix
getModelMatrix <- function(df) {
    return(model.matrix(~ . -1, df))
}

#' Find the number of different classes in labels.
#' @param train_y A vector
#' @return A number
getNumClass <- function(train_y) {
    return(length(unique(train_y)))
}

#' Find data frame columns where all values are the same
#' @param df A data frame
#' @return A character vector
getSingleValueCols <- function(df) {
    single_value_cols <- which(sapply(df, function(x) length(unique(x)) < 2))
    return(names(single_value_cols))
}

#' Get task specific parameters
#' @param task_type A string
#' @param train_y A vector
#' @return A list
getTaskParams <- function(task_type, train_y) {

    # Parameters are NULL by default
    task_params <- list(objective = NULL,
                        num_class = NULL)

    # Use reg:linear objective for regression task
    if (task_type == "regression") {
        task_params$objective <- "reg:linear"}

    # Use multi:softmax for classification.
    # Number of different classes needs to be defined.
    if (task_type == "classification") {
        task_params$objective <- "multi:softmax"
        task_params$num_class <- getNumClass(train_y)
    }

    return(task_params)
}


# TODO: get metric to use
#' Translate metric names for caret, xgboost, randomForest
#' @param metric A string
#' @param use_case A string
#' @return A string
getEvalMetric <- function(metric, use_case = "default") {

    use_case <- tolower(use_case)

    if(use_case == "default") {
        metric <- switch(metric,
                         accuracy = "error",
                         logLoss = "logLoss",
                         metric)
    } else if(use_case == "caret") {
        metric <- switch(metric,
                         accuracy = "Accuracy",
                         logLoss = "logLoss",
                         rmse = "RMSE",
                         metric)
    }

    return(metric)
}
