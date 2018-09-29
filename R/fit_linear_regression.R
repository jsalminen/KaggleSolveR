#' Get a linear regression model
#' @param train_x A data frame
#' @param train_y A vector
#' @param task_type A string
#' @param metric A string
#' @param cv_rounds A number
#' @return A model
getLinearRegression <- function(train_x, train_y, task_type = 'regression',
                                metric = NULL, cv_rounds = 5) {

    single_value_cols <- getSingleValueCols(train_x)

    train_x_linreg <- as.data.frame(getModelMatrix(train_x))

    linreg_formula <- getLinRegFormula(single_value_cols)

    linreg_model <- glm(train_y ~., data = train_x_linreg, family = 'gaussian')

    return(linreg_model)
}

#' Get a formula for linear regression
#' @param single_calue_cols A vector
#' @return A formula
getLinRegFormula <- function(single_value_cols) {
    form <- "train_y ~."

    for (col in single_value_cols) {
        form <- paste(form, "-", col)
    }
    return(as.formula(form))
}
