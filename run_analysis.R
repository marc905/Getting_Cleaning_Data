#Getting & Cleaning Data Course Assignment
#Marc Reitz

library(plyr)
library(dplyr)

if(!file.exists("./Project")) 
{dir.create("./Project")

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./Project/getdata%2Fprojectfiles%2FUCI HAR Dataset.zip")
unzip('./Project/getdata%2Fprojectfiles%2FUCI HAR Dataset.zip' , exdir = 'Project')
}

# Obtain attribute names from features.txt
features <- read.table("./Project/UCI HAR Dataset/features.txt", sep = " ")

# Read in test and training files
train_set <- read.table("./Project/UCI HAR Dataset/train/X_train.txt")   #Measurement Files
test_set <- read.table("./Project/UCI HAR Dataset/test/X_test.txt")
train_label <- read.table("./Project/UCI HAR Dataset/train/y_train.txt")
test_label <- read.table("./Project/UCI HAR Dataset/test/y_test.txt")
train_subject <- read.table("./Project/UCI HAR Dataset/train/subject_train.txt")
test_subject <- read.table("./Project/UCI HAR Dataset/test/subject_test.txt")

#1.  Merges the training and the test sets to create one data set.
merged_set <- rbind(train_set, test_set)
merged_label <- rbind(train_label, test_label)
merged_subject <- rbind(train_subject, test_subject)

#Set column names of merged set from attribute name file
names(merged_set) <- features[,2]

#2.  Extracts only the measurements on the mean and standard deviation for each measurement.  

Mean_STD_Measures <- merged_set[,grep("-(mean|std)\\(\\)", features[,2])]

#3.  Uses descriptive activity names to name the activities in the data set
names(merged_label) <- "Activities"
activity_label <- read.table("./Project/UCI HAR Dataset/activity_labels.txt", sep = " ")
activity_label <- rename(activity_label, Activities = V1, Activity = V2)

merged_label <- merge(merged_label, activity_label, by.x="Activities", by.y="Activities")

#4.  Appropriately labels the data set with descriptive variable names.

names(merged_subject) <- "Subjects"
Combined_Data_Set <- cbind(merged_subject, merged_label, Mean_STD_Measures)
Combined_Data_Set <- Combined_Data_Set[, -match("Activities", names(Combined_Data_Set))]

names(Combined_Data_Set)<- gsub("mean", "Mean", names(Combined_Data_Set))
names(Combined_Data_Set)<- gsub("std", "Standard_Deviation", names(Combined_Data_Set))
names(Combined_Data_Set)<- gsub("\\()", "", names(Combined_Data_Set))


#5. From the data set in step 4, creates a second, independent tidy data set 
#   with the average of each variable for each activity and each subject.

Independent_Tidy_Data_Set <- Combined_Data_Set %>% group_by(Subjects, Activity) %>% summarize_all(funs(mean))
write.table(Independent_Tidy_Data_Set, "./Project/Question_5_Answer.txt", row.names = FALSE)



