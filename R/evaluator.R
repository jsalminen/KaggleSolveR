
#' Predict and evaluate performance of a machine learning model
#' @param ml_model A machine learning model
#' @param model_name A string specifying the name of the model
#' @param valid_x A data frame containing the features
#' @param valid_y A data frame containing the correct answers
#' @param metric A string containing the metric to be used
#' @return Score on prediction
getEvaluation <- function(ml_model, model_name, valid_x, valid_y, metric) {

    valid_x <- fixDataRepresentation(valid_x, model_name)

    pred <-  predict(ml_model, newdata = valid_x)

    result <- NULL

    if (metric == "accuracy") result <- Metrics::accuracy(valid_y, pred)
    else if (metric == "rmse") result <- Metrics::rmse(valid_y, pred)

    return(result)
}

#' Create submission data frame
#' @export
#' @param final_model A machine learning model
#' @param model_name A string
#' @param test_x A data frame containing the test features
#' @param Id A vector containing the submission Ids
#' @return A data frame
getSubmission <- function(final_model, model_name, test_x, Id) {

    test_x <- fixDataRepresentation(test_x, model_name)

    pred <- predict(final_model, newdata = test_x)
    submission <- data.frame(Id = Id, pred = pred)

    return(submission)
}



