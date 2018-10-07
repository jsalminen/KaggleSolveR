


#' Select the best performing model
#' @export
#' @param train_x A data frame for training features
#' @param train_y A data frame for training targets
#' @param valid_x A data frame for validation features
#' @param valid_y A data frame for validation targets
#' @param metric A string for metric type
#' @param minimize_score A boolean
#' @param best_fit_model An optional list containing an earlier best model
#' @param model_list An optional vector of models to fit
#' @param verbose A boolean
#' @return A list containing the best fit model
selectModel <- function(train_x, train_y, valid_x, valid_y,
                        task_type, metric,
                        minimize_score = TRUE,
                        best_fit_model = NULL,
                        model_list = NULL,
                        verbose = TRUE) {

    if (is.null(model_list)) model_list <- getModelList(task_type)
    if (is.null(best_fit_model)) best_fit_model <- list()

    for (model_name in model_list) {

        if (verbose) cat("Training ", model_name, "...\n")

        new_model <- getModel(train_x, train_y,
                              task_type, metric, model_name)

        new_score_train <- getEvaluation(new_model, model_name,
                                         train_x, train_y, metric)

        new_score <- getEvaluation(new_model, model_name,
                                   valid_x, valid_y, metric)

        best_fit_model <- updateBestModel(new_score, new_score_train,
                                          model_name, new_model,
                                          best_fit_model, minimize_score)

        if (verbose) {
            cat("Train score: ", new_score_train, "\n")
            cat("Test score: ", new_score, "\n\n")
        }
    }

    return(best_fit_model)
}


#' Fit all training data using the best model with optimized parameters
#' @export
#' @param train_x A data frame containing the training features
#' @param train_y A data frame containing the training targets
#' @param best_fit_model A list containing the best fit model
#' @param task_type A string
#' @param metric A string
#' @return A model
fitFinalModel <- function(train_x, train_y,
                          best_fit_model,
                          task_type,
                          metric) {

    if (tolower(best_fit_model$best_model_name) == "xgboost") {
        final_model <- fitFinalXgboost(train_x, train_y,
                                       task_type, metric,
                                       best_fit_model)
    } else {
        final_model <- NULL
    }

    return(final_model)
}

#' Get a list of models to fit
#' @param task_type A string, "classification" or "regression"
#' @return A vector of models
getModelList <- function(task_type) {

    task_type <- tolower(task_type)

    CLASSIFICATION_MODELS <- c("XGBoost",
                               "RandomForest",
                               "Lasso",
                               "LogisticRegression")
    REGRESSION_MODELS <- c("XGBoost",
                           "RandomForest",
                           "Lasso",
                           "LinearRegression")

    if (task_type == 'classification') return(CLASSIFICATION_MODELS)
    else if (task_type == 'regression') return(REGRESSION_MODELS)
    else return(c())
}

#' Get an optimized machine learning model
#' @param train_x A data frame containing the training features
#' @param train_y A data frame containing the training targets
#' @param task_type A string
#' @param metric A string
#' @param model_name A string
#' @return A model object
getModel <- function(train_x, train_y,
                     task_type, metric,
                     model_name) {

    model_name <- tolower(model_name)

    if (model_name == 'xgboost') {
        new_model <- getTunedXgboost(train_x, train_y, task_type,
                                     metric = "logLoss")
    }
    else if (model_name == 'randomforest') {
        new_model <- getRandomForest(train_x, train_y, task_type, metric)
    }
    else if (model_name == "lasso") {
        new_model <- getLasso(train_x, train_y, task_type, metric)
    }
    else if (model_name == "logisticregression") {
        new_model <- getLogisticRegression(train_x, train_y)
    }
    else if (model_name == "linearregression") {
        new_model <- getLinearRegression(train_x, train_y)
    }

    return(new_model)
}

#' Update best_fit_model if the new model is better than the current best model
#' @param new_score A number
#' @param new_score_train A number
#' @param new_model_name A string
#' @param new_model A model
#' @param best_fit_model A list containing the current best model
#' @param minimize_score A boolean
#' @return A list containing the best fit model
updateBestModel <- function(new_score, new_score_train,
                            new_model_name, new_model,
                            best_fit_model, minimize_score) {

    best_score <- best_fit_model[["best_score"]]

    if (isBetter(new_score, best_score, minimize_score)) {
        best_fit_model$best_model_name <- new_model_name
        best_fit_model$best_score <- new_score
        best_fit_model$best_score_train <- new_score_train
        best_fit_model$best_model <- new_model
    }

    return(best_fit_model)
}

#' Check if the new score is better than the current best score
#' @param new_score A number
#' @param best_score A number
#' @param minimize A boolean
#' @return A boolean
isBetter <- function(new_score, best_score, minimize_score) {
    if (is.null(new_score)) return(FALSE)
    if (is.null(best_score)) return(TRUE)

    if (minimize_score) {
        result <- new_score < best_score
    } else {
        result <- new_score > best_score
    }

    return (result)
}





