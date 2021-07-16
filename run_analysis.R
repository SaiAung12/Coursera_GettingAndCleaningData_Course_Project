install.packages("dplyr")
library(dplyr)

# 1.Merges the training and the test sets to create one data set.
## Download and unzip dataset

if(!file.exists("./dataset")){dir.create("./dataset")}

downloadUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(downloadUrl, destfile = "./dataset/download.zip")

unzip(zipfile = "./dataset/download.zip", exdir = "./dataset")

## Read training data
x_train <- read.table("./dataset/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./dataset/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./dataset/UCI HAR Dataset/train/subject_train.txt")


## Read Testing data
x_test <- read.table("./dataset/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./dataset/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./dataset/UCI HAR Dataset/test/subject_test.txt")

## Read feature data
features <- read.table('./dataset/UCI HAR Dataset/features.txt')

## Read labels data
labels <- read.table('./dataset/UCI HAR Dataset/activity_labels.txt')

## Put column names
colnames(x_train) <- features[,2]
colnames(y_train) <-"ActivityId"
colnames(subject_train) <- "SubjectId"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "ActivityId"
colnames(subject_test) <- "SubjectId"

colnames(labels) <- c('ActivityId','ActivityType')

## Merge the data according to testing and training 
train_data <- cbind(y_train, subject_train, x_train)
test_data <- cbind(y_test, subject_test, x_test)
combined_data <- rbind(train_data, test_data)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## Take only mean and std data
filtered_data <- combined_data %>% select(SubjectId, ActivityId, contains("mean"), contains("std"))

# 3. Uses descriptive activity names to name the activities in the data set.
## Apply descriptive activity names to the activities
filtered_data$ActivityId <- labels[filtered_data$ActivityId, 2]

# 4. Appropriately labels the data set with descriptive variable names.
## Apply descriptive variable names
colnames(filtered_data) <- gsub("^t", "Time", colnames(filtered_data))
colnames(filtered_data) <- gsub("tBody", "TimeBody", colnames(filtered_data))
colnames(filtered_data) <- gsub("Acc", "Accelerometer", colnames(filtered_data))
colnames(filtered_data) <- gsub("[()]", "", colnames(filtered_data))
colnames(filtered_data) <- gsub("-mean", "Mean", colnames(filtered_data))
colnames(filtered_data) <- gsub("Gyro", "Gyroscope", colnames(filtered_data))
colnames(filtered_data) <- gsub("Mag", "Magnitude", colnames(filtered_data))
colnames(filtered_data) <- gsub("^f|Freq", "Magnitude", colnames(filtered_data))
colnames(filtered_data) <- gsub("angle", "Angle", colnames(filtered_data))
colnames(filtered_data) <- gsub("gravity", "Gravity", colnames(filtered_data))
colnames(filtered_data) <- gsub("std", "STD", colnames(filtered_data))

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## Create a second, independent tidy dataset
final_data <- filtered_data %>% group_by(SubjectId, ActivityId) %>% summarise_all(list(mean = mean, median = median))
write.table(final_data, "final_data.txt", row.name=FALSE)

