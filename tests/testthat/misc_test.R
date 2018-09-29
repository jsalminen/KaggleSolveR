
col_classes <- c("factor",
                 "factor",
                 "numeric",
                 "numeric",
                 "character",
                 "character",
                 "numeric",
                 "numeric",
                 "numeric",
                 "character",
                 "character",
                 "factor",
                 "factor",
                 "numeric",
                 "numeric",
                 "Date",
                 "Date",
                 "factor",
                 "factor")

train <- read.csv2("cleanData_train.csv",
                   colClasses = col_classes)

config_file <- createConfigFile(train)
write.csv2(config_file, "config_file_template.csv", row.names = FALSE)

