
#' Get a logistic regression model
#' @param train_x A data frame
#' @param train_y A vector
#' @param task_type A string
#' @param metric A string
#' @return A model
getLogisticRegression <- function(train_x, train_y, task_type='classification',
                                  metric=NULL) {

    logreg_model <- nnet::multinom(train_y ~., data = train_x, trace = FALSE)

    return(logreg_model)
}
