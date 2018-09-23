
#' Identify the type of the learning task
#' @param target_col A vector
#' @return A string
identifyTask <- function(target_col) {

    target_class <- class(target_col)

    if (target_class == "numeric") {
        return("regression")
    } else {
        return("classification")
    }

}
