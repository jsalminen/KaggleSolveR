

#' Get an optimized Lasso model
#' @param train_x A data frame
#' @param train_y A vector
#' @param task_type A string
#' @param metric A string
#' @param cv_rounds A number
#' @return A model
getLasso <- function(train_x, train_y, task_type, metric, cv_rounds = 5) {

    task_params <- getTaskParams(task_type, train_y)

    train_x_lasso <- getModelMatrix(train_x)

    train_control <- getLassoTrainControl(task_type, cv_rounds)

    eval_metric_caret <- getEvalMetric(metric, "caret")

    lasso_model <- tuneLasso(train_x_lasso, train_y,
                             task_type, task_params, train_control,
                             eval_metric_caret)

    return(lasso_model)
}

#' Optimize a Lasso model
#' @param train_x A data frame
#' @param train_y A vector
#' @param task_type A string
#' @param task_params A list
#' @param train_control A train control object
#' @param metric A string
#' @return A model
tuneLasso <- function(train_x, train_y,
                      task_type, task_params, train_control,
                      metric) {

    if (task_type == 'regression') {
        lasso_model <- caret::train(x=train_x,
                                    y=train_y,
                                    metric=metric,
                                    method='glmnet',
                                    trControl=train_control)
    }
    else if (task_type == 'classification') {
        lasso_model <- caret::train(x=as.matrix(train_x),
                                    y=train_y,
                                    metric=metric,
                                    method='glmnet',
                                    trControl=train_control)
    }

    return(lasso_model)
}

#' Get train control for the Lasso model
#' @param task_type A string
#' @param cv_rounds A number
#' @return A train control object
getLassoTrainControl <- function(task_type, cv_rounds) {

    if (task_type == 'regression') {
        control <- caret::trainControl(method='cv',
                                       number=cv_rounds,
                                       search='random')

    } else if (task_type == 'classification') {
        control <- caret::trainControl(method='cv',
                                       number=cv_rounds,
                                       search='random')
    }

}

