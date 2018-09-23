#' Get an optimized random forest model
#' @param train_x A data frame
#' @param train_y A vector
#' @param task_type A string
#' @param metric A string
#' @param cv_rounds A number
#' @return A model
getRandomForest <- function(train_x, train_y,
                            task_type, metric = NULL,
                            cv_rounds = 5) {


    train_control <- getRFTrainControl(cv_rounds)

    rdm_model <- tuneRandomForest(train_x,
                                  train_y,
                                  metric,
                                  train_control)

    return(rdm_model)
}

#' Get train control for a random forest model
#' @param cv_rounds A number
#' @return A train control object
getRFTrainControl <- function(cv_rounds) {
    control <- caret::trainControl(method='repeatedcv',
                                   number=cv_rounds,
                                   repeats=3,
                                   search='random')
    return(control)
}

#' Optimize a random forest model
#' @param train_x A data frame
#' @param train_y A vector
#' @param metric A string
#' @param train_control A train control object
tuneRandomForest <- function(train_x, train_y,
                             metric,
                             train_control) {

    rf_model <- caret::train(x=train_x,
                             y=train_y,
                             method='rf',
                             metric=metric,
                             trControl=train_control)

    return(rf_model)
}



