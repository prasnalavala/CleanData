# The purpose of this project is to gather raw data from wearable accelerometers from 
# Samsung Galaxy S smartphone and perform applicable functions on the training and test datasets
# to form one simple dataset of only mean and standard deviation of each measurement.

library(dplyr)

# Check if the file exists, otherwise download and unzip the compressed folder
# containing training and test files along with other relevant descriptive files

if (!file.exists("./Project")){
        dir.create("./Project")
}
zipfileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(zipfileURL,destfile = "./Project/HARdatazip.zip")

if (!file.exists("./Project/UCI HAR Dataset")) {
        unzip("./Project/HARdatazip.zip", exdir="./Project")
}

# Now that the compressed folder is downloaded and unzipped, we will read the data into R and view it

# Load the training datasets, view names and combine columns
trainset <- read.table("./Project/UCI HAR Dataset/train/X_train.txt")
trainLabels <- read.table("./Project/UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("./Project/UCI HAR Dataset/train/subject_train.txt")
names(trainset); names(trainLabels); names(trainSubjects)
trainingdata <- cbind(trainSubjects, trainLabels, trainset)

# Load the test datasets, view names and combine columns 
testset <- read.table("./Project/UCI HAR Dataset/test/X_test.txt")
testLabels <- read.table("./Project/UCI HAR Dataset/test/y_test.txt")
testSubjects <- read.table("./Project/UCI HAR Dataset/test/subject_test.txt")
names(testset); names(testLabels); names(testSubjects)
testdata <- cbind(testSubjects, testLabels, testset)

# Load activity labels and view 
activityLabels <- read.table("./Project/UCI HAR Dataset/activity_labels.txt")
activityLabels
activityLabels[,2] <- as.character(activityLabels[,2])

# Load features and view
features <- read.table("./Project/UCI HAR Dataset/features.txt")
features
features[,2] <- as.character(features[,2])


# After all the information is loaded and prepped, we merge training and test datasets
allData <- rbind(trainingdata, testdata)
head(allData)
names(allData)

# We then give the newly combined dataset column names
colnames(allData) <- c("subject","activity", features[,2])
colnames(allData)

# grep(".*mean.*|.*std.*", allData[,2])

# keep only those columns that have mean or standard deviation values 
# and assign that to a new dataframe. Compare dimensions of the original dataframe
# and the new dataframe which is a subset of the original combined dataframe.

allData2 <- allData[,grepl("subject|activity|mean|std", colnames(allData))]
dim(allData2)

# since the dataframes are large, we will remove the training and test datasets to save space

rm(testLabels); rm(testSubjects); rm(testset);rm(trainLabels); rm(trainSubjects); rm(trainset)

# We can also remove testdata and training data dataframes, but I wanted to keep them for comparison of transformations


# reassign activities & subjects from integer class to into descriptive factors for easy readability of data
allData2$activity <- factor(allData2$activity, levels = activityLabels[,1], labels = activityLabels[,2])
allData2$subject <- as.factor(allData2$subject)
class(allData$activity); class(allData2$activity)
class(allData$subject); class(allData2$subject)

library(reshape2)
allData.melted <- melt(allData2, id = c("subject", "activity"))
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)

newcols <- colnames(allData.mean)
newcols <- gsub("[\\(\\)-]", "", newcols)

# expand abbreviations and clean up names
newcols <- gsub("^f", "frequencyDomain", newcols)
newcols <- gsub("^t", "timeDomain", newcols)
newcols <- gsub("Acc", "Accelerometer", newcols)
newcols <- gsub("Gyro", "Gyroscope", newcols)
newcols <- gsub("Mag", "Magnitude", newcols)
newcols <- gsub("Freq", "Frequency", newcols)
newcols <- gsub("mean", "Mean", newcols)
newcols <- gsub("std", "StandardDeviation", newcols)
newcols <- gsub("BodyBody", "Body", newcols)

# use new labels as column names
colnames(allData.mean) <- newcols
View(allData.mean)

# Output the tidy data file into a text document
write.table(allData.mean, "./Project/tidy_data.txt", row.names = FALSE, quote = FALSE)
