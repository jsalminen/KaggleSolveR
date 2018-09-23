

plot_distributions <- function(df, skipcols = c()) {
    
    for(n in names(df)) {
        if (!(n %in% skipcols)) {
            cat("##", n, "\n")
            if (class(df[[n]]) == "factor") {
                print(ggplot(df, aes_string(x=n, fill = "target")) +
                          geom_bar(position = "dodge"))
            }
            else {
                print(ggplot(df, aes_string(x=n, fill = "target")) +
                          geom_histogram(position = "dodge"))
            }
            cat(" \n\n")
            print(summary(df[[n]]))
            cat("\nMissing values:", sum(is.na(df[[n]])))
            
            cat(' \n \n')
        }
    }
}
plot_correlations <- function(df, skipcols = c()) {
    col_names <- names(df)
    corr_cols <- c()
    for (n in col_names) {
        if (class(df[[n]]) == "numeric" & !(n %in% skipcols)) {
            corr_cols <- c(corr_cols, n)
        }
    }
    correlations <- (cor(df[, corr_cols, with = FALSE]))
    correlations[upper.tri(correlations, diag = TRUE)] <- NA
    correlations_melt <- data.table::melt(correlations, na.rm = TRUE)
    correlations <- round(cor(df[, corr_cols, with = FALSE]), 2)
    correlations[upper.tri(correlations, diag = TRUE)] <- ""
    names(correlations_melt) <- c("Var1", "Var2", "value")
    cor_plot <- ggplot(correlations_melt, aes(Var2, Var1, fill = value)) + 
        geom_tile(color = 'white') +
        scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white',
                             midpoint = 0, limit = c(-1, 1), space = 'Lab',
                             name = 'Correlation') + 
        theme_minimal(12) +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
        scale_y_discrete(limits = rev(levels(correlations_melt$Var1))) +
        coord_fixed()
        
    print(cor_plot)
    
    print(knitr::kable(correlations))
    
    
}

plot_missing_values <- function(df, target_col, skipcols = c()) {
    cols <- names(df)[!(names(df) %in% skipcols)]
    df_missing <- df[, lapply(.SD, function(x) (sum(is.na(x)) / .N)), 
                     by = get(target_col),
                     .SDcols = cols]
    
    df_missing <- melt(df_missing, id.vars = c("get"))
    
    p <- ggplot(df_missing, aes(variable, get, fill = value)) + geom_raster() +
        ylab(target_col) +
        theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
        scale_fill_continuous(name = "% missing")
    
    print(p)
}

