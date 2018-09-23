

# Input: train_x, train_y
# Output: vector of feature names to use

selectFeatures <- function(train_x, train_y, task_type, min_importance = 0.01) {
    
    `%>%` <- magrittr::`%>%`
    
    rmd_model <- randomForest::randomForest(train_x, y = train_y, 
                                            importance = TRUE,
                                            ntree = 100)
    importances <- getImportances(rdm_model, task_type)
    
    importances <- importances %>%
        dplyr::filter(percentage > min_importance)
        
    return(importances$variable)
}

getImportances <- function(rdm_model, task_type) {
    if (task_type == "classification") {
        importances <- as.data.frame(rmd_model$importance)
        importances$variable <- row.names(importances)
        total_gini <- sum(importances$MeanDecreaseGini)
        importances$percentage <- importances$MeanDecreaseGini / total_gini
    } else if (task_type == "regression") {
        importances <- as.data.frame(rmd_model$importance)
        importances$variable <- row.names(importances)
        total_IncMSE <- sum(importances[,"%IncMSE"])
        importances$percentage <- importances[, "%IncMSE"] / total_IncMSE
    }
    return(importances)
}
