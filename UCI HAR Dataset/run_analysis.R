#------------------------------------------------------------------------------#
#                        DAMIEN DOS SANTOS LUIS                                #
#             GETTING AND CLEANING DATA COURSE ON COURSERA                     #
#                               PEER REVIEW                                    #
#                               20-02-17                                       #
#------------------------------------------------------------------------------#

run_analysis <- function() {
        
        library(plyr)
        library(dplyr)
        library(reshape2)
        library(matrixStats)
        
        training <- read.table("train/X_train.txt")
        head(training)
        str(training)
        
        testing <- read.table("test/X_test.txt")
        head(testing)
        str(testing)
        
        # Merging the two datasets and some check controls
        ncol(training) == ncol(testing)
        nrow(merging) == nrow(testing) + nrow(training)
        merged.data <- rbind(training, testing)
        
        features <- read.table("features.txt")
        colnames(merged.data) <- features[,2]
        
        # Extract columns containin mean and sdt
        
        index.selection <- grep(".mean|.std", colnames(merged.data))
        subset.data <- merged.data[,index.selection]
        colnames(subset.data) <- gsub("[()]","", colnames(subset.data))
        
        # Uses descriptive activity names to name the activities in the data set
        # Extract the classes for each group and add the corresponding activity label
        
        # Takes two tables, renames the column and binds them
        ## !!! Function adapted to the project's datasets !!! 
        
        merge.tables <- function(table1, table2, column.name) {
                read.table1 <- read.table(table1)
                colnames(read.table1) <- column.name
                read.table2 <- read.table(table2)
                colnames(read.table2) <- column.name  
                
                rbind(read.table1, read.table2)
        }
        
        ## Merging tables containing information about subjets, their activity and their quantitative measurements
        activity.labels <- read.table("activity_labels.txt")
        colnames(activity.labels) <- c("class", "activity")
        
        all.labels.activity <- merge(merge.tables("train/y_train.txt", "test/y_test.txt", "class"), activity.labels, by = "class")
        merging.labelled <- cbind(all.labels.activity, merge.tables("train/subject_train.txt" , "test/subject_test.txt", "subject"), subset.data)
        
        
        # From the data set in previous step, creates a second, independent tidy data set with 
        # the average of each variable for each activity and each subject.
        
        summarized.table <- merging.labelled %>% group_by(activity, subject) %>%  summarise_each(funs(mean(.)))
        str(summarized.table)
        write.table(summarized.table, "summarized_dataset_table.txt", row.names = FALSE, quote = FALSE)
        
        write.table(colnames(summarized.table), "index_summarized_table.txt", quote = FALSE, row.names = FALSE)
        
}

