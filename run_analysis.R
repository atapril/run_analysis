library(data.table)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# download file
download.file(url,'./UCI HAR Dataset.zip', mode="wb")
#unzip file
unzip("UCI HAR Dataset.zip", exdir = getwd())
# read features.txt, names it "features"
features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
# only need one column in features
features <- features[,2]
# make features character, so it can become name of column in train data and test data
features <-  as.character(features)

# make "train" data table
train.set <- read.csv("./UCI HAR Dataset/train/X_train.txt", header = FALSE, sep = "")
train.label <- read.csv("./UCI HAR Dataset/train/y_train.txt", header = FALSE, sep = " ")
train.subject <- read.csv("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, sep = "")
train <- data.frame(train.subject, train.label, train.set)
# name all the columns in "train" data table
names(train) = c(c("Subject", "Activities"), features)

# make test data table
test.set <- read.csv("./UCI HAR Dataset/test/X_test.txt", header = FALSE, sep = "")
test.label <- read.csv("./UCI HAR Dataset/test/y_test.txt", header = FALSE, sep = "")
test.subject <- read.csv("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, sep = "")
test <- data.frame(test.subject, test.label, test.set)
names(test) <- c(c("Subject","Activities"), features)

# 1. Merge train and test sets to creat one data set
DataSet <- rbind(train, test)

# 2. Extract only the measurements on the mean and standard deviation for each measurement
MeanAndstd <- grep("mean|std", features)
DataExtract <- DataSet[,c(1,2, MeanAndstd +2)]

# 3. Uses descriptive activity names to name the activities in the data set
activity_labels <- read.csv("./UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "")

for (i in 1:6){
        DataSet$Activities <- gsub(activity_labels$V1[i], activity_labels$V2[i], DataSet$Activities, ignore.case = TRUE)
}
# 4. Appropriately labels the data set with descriptive variable names
Name <- names(DataSet)
Name <- gsub("^t", "Time ", Name)
Name <- gsub("Acc", " Aceleration", Name)
Name <- gsub("-", " ", Name)
Name <- gsub("[(][)]", "",Name)
Name <- gsub("mean", "Mean ", Name)
Name <- gsub("std", "Standard deviation", Name) 
Name <- gsub("mad", "Median absolute deviation", Name)
Name <- gsub("max", "Max", Name)
Name <- gsub("min", "Min", Name)
Name <- gsub( "sma", "SMA", Name)
Name <- gsub("iqr", "Interquartile range", Name)
Name <- gsub("Gyro", " Gyroscope", Name)
Name <- gsub("Mag", " Magnitude", Name)
Name <- gsub("^f", "Frequency ", Name)
names(DataSet) <- Name

#5 From Date set in #4, create a tidy data set with the average of each variable for each activity and each subject.
TidyDataSet <- aggregate(DataSet[,3:563], list( Activities = DataSet$Activities, Subject=DataSet$Subject),mean)
