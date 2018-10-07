

#' Get an optimized Xgboost model
#' @param train_x A data frame
#' @param train_y A vector
#' @param task_type A string
#' @param metric A string
#' @param cv_rounds A number
#' @return A model
getTunedXgboost <- function(train_x, train_y, task_type, metric = NULL,
                       cv_rounds = 5) {

    # Get task parameters based on task type
    task_params <- getTaskParams(task_type, train_y)

    # Get evaluation method
    eval_metric <- getEvalMetric(metric)
    eval_metric_caret <- getEvalMetric(metric, "caret")

    # Get training control parameters.
    train_control <- getTrainControl(cv_rounds, task_type)

    # Prepare data: put features in a sparse matrix
    train_x_sparse <- getSparseMatrix(train_x)

    # Prepare data for caret: format labels
    train_y_tuning <- fixLabels(train_y, task_type)

    # Parameter tuning

    bestTune <- NULL

    # Run 4 rounds of parameter tuning.
    for (i in 1:4) {

        # Get tuning parameter grid for current round.
        # Use previous round best parameters as a basis, if available
        param_grid <- getParamGrid(bestTune, round = i)

        # Run parameter tuning with current parameters
        xgb_model <- tuneModel(train_x_sparse,
                               train_y_tuning,
                               method = "xgbTree",
                               task_params,
                               train_control,
                               param_grid,
                               eval_metric_caret,
                               task_type)

        # Extract the best parameters for the next round
        bestTune <- xgb_model$bestTune
    }

    # Decrease learning rate
    bestTune$eta <- 0.01

    # Fit final model with best parameters, lower learning rate, and moar trees
    xgb_model <- runXgboost(train_x_sparse,
                            train_y,
                            task_params,
                            task_type = task_type,
                            params = as.list(bestTune),
                            metric = eval_metric,
                            nrounds = 5000)

    # Return final model
    return(xgb_model)
}

#' Train Xgboost with full data and optimized parameters
#' @param train_x A data frame
#' @param train_y A vector
#' @param task_type A string
#' @param metric A string
#' @param best_fit_model A list containing the tuned xgboost model
#' @return A model
fitFinalXgboost <- function(train_x, train_y, task_type, metric = NULL,
                            best_fit_model) {

    # Get task parameters based on task type
    task_params <- getTaskParams(task_type, train_y)

    # Get evaluation method
    eval_metric <- getEvalMetric(metric)

    # Prepare data: put features in a sparse matrix
    train_x_sparse <- getSparseMatrix(train_x)

    temp <- best_fit_model$best_model$params

    params <- list(max_depth = temp$max_depth,
                   eta = temp$eta,
                   gamma = temp$gamma,
                   colsample_bytree = temp$colsample_bytree,
                   min_child_weight = temp$min_child_weight,
                   subsample = temp$subsample)

    xgb_model <- runXgboost(train_x_sparse,
                            train_y,
                            task_params,
                            task_type = task_type,
                            params = params,
                            metric = eval_metric,
                            nrounds = 5000)

    return(xgb_model)
}

#' Run parameter tuning for Xgboost using caret package
#' @param train_x A data frame
#' @param train_y A vector
#' @param method A string
#' @param task_params A list
#' @param train_control A train control object
#' @param param_grid A data frame
#' @param metric A string
#' @param task_type A string
#' @return A model
tuneModel <- function(train_x,
                      train_y,
                      method,
                      task_params,
                      train_control,
                      param_grid,
                      metric,
                      task_type) {

    # Run tuning for classification task
    if (task_type == "classification") {
        xgb_model <- caret::train(
            x=train_x,
            y=train_y,
            metric=metric,
            trControl=train_control,
            tuneGrid=param_grid,
            method=method,
            objective=task_params$objective,
            num_class=task_params$num_class
        )
    }
    # Run tuning for regression task
    else if (task_type == "regression") {
        xgb_model <- caret::train(
            x=train_x,
            y=train_y,
            metric=metric,
            trControl=train_control,
            tuneGrid=param_grid,
            method=method,
            objective=task_params$objective
        )
    }

    return(xgb_model)
}

#' Run full model with given parameters. Stop early, if model stops improving.
#' @param train_x_sparse A sparse model matrix
#' @param train_y A vector
#' @param task_params A list
#' @param task_type A string
#' @param params A list
#' @param nrounds A number
#' @param metric A string
#' @param method A string
#' @return A model
runXgboost <- function(train_x_sparse,
                       train_y,
                       task_params,
                       task_type,
                       params,
                       nrounds = 5000,
                       metric = NULL,
                       method = "xgbTree") {

    if (task_type == "classification") {
        xgb_model <- xgboost::xgboost(train_x_sparse,
                             label = as.numeric(as.character(train_y)),
                             params = params,
                             nrounds = nrounds,
                             objective = task_params$objective,
                             num_class = task_params$num_class,
                             metric = metric,
                             method = method,
                             early_stopping_rounds = 50,
                             verbose = 0)
    }

    else if (task_type == "regression"){
        xgb_model <- xgboost::xgboost(train_x_sparse,
                             label = train_y,
                             params = params,
                             nrounds = nrounds,
                             objective = task_params$objective,
                             metric = metric,
                             method = method,
                             early_stopping_rounds = 50,
                             verbose = 0)
    }

    return(xgb_model)
}

#' Get Xgboost tuning parameters to try next
#' @param params A list
#' @param round A number
#' @return A list
getParamGrid <- function(params = NULL, round) {

    # Default parameters, used if nothing is given
    if (is.null(params)) {
        params <- list(nrounds = 100,
                       max_depth = 5,
                       eta = 0.1,
                       gamma = 0,
                       colsample_bytree = 0.8,
                       min_child_weight = 1,
                       subsample = 0.8)
    }

    # Round 1. Find suitable number of trees for high learning rate (eta = 0.2)
    if (round == 1) {
        param_grid <- expand.grid(
            nrounds=seq(20, 80, 10),
            max_depth=5,
            eta=0.2,
            gamma=0,
            colsample_bytree=0.8,
            min_child_weight=1,
            subsample = 0.8
        )
    }
    # Round 2. Tune max_depth and min_child_weight
    else if (round == 2) {
        param_grid <- expand.grid(
            nrounds=params$nrounds,
            max_depth=seq(3,10,2),
            eta=0.2,
            gamma=0,
            colsample_bytree=0.8,
            min_child_weight=seq(1,6,2),
            subsample = 0.8
        )
    }
    # Round 3. Tune gamma.
    else if (round == 3) {
        param_grid <- expand.grid(
            nrounds=params$nrounds,
            max_depth=params$max_depth,
            eta=0.2,
            gamma=seq(0, 0.5, 0.1),
            colsample_bytree=0.8,
            min_child_weight=params$min_child_weight,
            subsample = 0.8
        )
    }
    # Round 4. Tune colsample_bytree and subsample.
    else if (round == 4) {
        param_grid <- expand.grid(
            nrounds=params$nrounds,
            max_depth=params$max_depth,
            eta=0.2,
            gamma=params$gamma,
            colsample_bytree=seq(0.6, 1, 0.1),
            min_child_weight=params$min_child_weight,
            subsample = seq(0.4, 1, 0.1)
        )
    }

    return(param_grid)
}


#' Pack training control parameters for Xgboost model
#' @param cv_rounds A number
#' @param task_type A string
#' @return A train control object
getTrainControl <- function(cv_rounds, task_type) {

    # Classification task
    if (task_type == "classification") {
        train_control <- caret::trainControl(
            method="cv",
            number=cv_rounds,
            verboseIter = FALSE,
            returnData = FALSE,
            classProbs = TRUE,
            summaryFunction = caret::multiClassSummary
        )
    }

    # Regression task
    else if (task_type == "regression") {
        train_control <- caret::trainControl(
            method="cv",
            number=cv_rounds,
            verboseIter = FALSE,
            returnData = FALSE,
            summaryFunction = caret::defaultSummary
        )
    }

    else { train_control <- NULL }

    return(train_control)
}



#' Fix label names for classification task
#' @param train_y A vector
#' @param task_type A string
#' @return A vector
fixLabels <- function(train_y, task_type) {

    # If task type is classification, change factor levels to valid names.
    if (task_type == "classification") {
        levels(train_y) <- make.names(levels(train_y))
    }

    return(train_y)
}

