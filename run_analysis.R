#File
#   run_analysis.R
#
# OVERVIEW
#   Using data collected from the accelerometers from the Samsung Galaxy S 
#   smartphone, work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".
#   See README.md for details.

library(dplyr)

#Get Data
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "UCI HAR Dataset.zip"

download.file(zipUrl, zipfile, mode = "wb")

#Unzip zip file 
datapath <- "UCI HAR dataset"
unzip(zipfile)

#read data
##training data
trainingSubjects <- read.table(file.path(datapath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(datapath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(datapath, "train", "y_train.txt"))


##test data
testSubjects <- read.table(file.path(datapath, "test", "subject_test.txt"))
testValues <- read.table(file.path(datapath, "test", "X_test.txt"))
testActivity <- read.table(file.path(datapath, "test", "y_test.txt"))

##read features
features <- read.table(file.path(datapath, "features.txt"), as.is = TRUE)

## read activity labels 
activities <- read.table(file.path(datapath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

#Step 1: Merging training and test set to creat one data set

#make single data table

humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables 
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")


#Step 2: Extract only the measumeant on the mean and STD

# determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

# keep data in these columns only
humanActivity <- humanActivity[, columnsToKeep]

# Step 3: Use discriptive activity names to name activities in data

humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

# Step 4 Appropriately label the data set with discriptive variable names

# get column names
humanActivityCols <- colnames(humanActivity)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols

#Step 5: create tidy set with average of each variable for each activity 

# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)