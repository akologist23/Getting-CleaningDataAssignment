#Extracting downloading data files
zipURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(zipURL,destfile = "Smartphone.zip")
unzip("Smartphone.zip",exdir="/.")

#Extract "test" files and name columns
Xtest <- read.table("/UCI HAR Dataset/test/X_test.txt",header=FALSE, sep="")

ytest <- read.table("/UCI HAR Dataset/test/y_test.txt",header=FALSE, sep="")
names(ytest) <- "activityID"

subjectTest <-read.table("/UCI HAR Dataset/test/subject_test.txt",header=FALSE, sep="")
names(subjectTest) <- "subjectID"

#Extract "train" files and name columns

Xtrain <- read.table("/UCI HAR Dataset/train/X_train.txt",header=FALSE, sep="")

ytrain <- read.table("/UCI HAR Dataset/train/y_train.txt",header=FALSE, sep="")
names(ytrain) <- "activityID"

subjectTrain <- read.table("/UCI HAR Dataset/train/subject_train.txt",header=FALSE, sep="")
names(subjectTrain) <- "subjectID"

#Extract "features" and "labels" and name columns

features <-read.table("/UCI HAR Dataset/features.txt",header=FALSE, sep="",stringsAsFactors = FALSE)
names(features) <- c("featureID","feature")

labels <-read.table("/UCI HAR Dataset/activity_labels.txt",header=FALSE, sep="",stringsAsFactors = FALSE)
names(labels) <- c("activityID","activity")


#part of OBJECTIVE 4 - add feature names to "test" and "train" datasets

names(Xtest) <- features$feature
names(Xtrain) <- features$feature

#OBJECTIVE 1 - "Merges the training and the test sets to create one data set."
#a) explore data for further manipulation
#b) adds "set", "subject", and "activityID" column to each respective data.frame identifying data as "test" or "train"
#c) row-bind data together into one object

#a
class(Xtest) # class = data.frame
class(Xtrain) # class = data.frame

table(subjectTest)
table(subjectTrain)
table(ytest)
table(ytrain)

#b
Xtest$set <- "test"
Xtrain$set <- "train"

Xtest$subject <- subjectTest$subjectID
Xtrain$subject <- subjectTrain$subjectID

Xtest$activityID <- ytest$activityID
Xtrain$activityID <- ytrain$activityID

#c
Xtesttrain <- rbind.data.frame(Xtrain,Xtest)


#OBJECTIVE 2 - "Extracts only the measurements on the mean and standard deviation for each measurement. "
#selecting all variables with "mean()" or "std()" in the name. Did not extract measurements on meanFreq() or mean values used to define the angle() measurement

sel<- grep("mean[()]|std[()]",names(Xtesttrain),value=TRUE)
length(sel)


Xmeanstd <- Xtesttrain[,c("set","subject","activityID",sel)]

#OBJECTIVE 3 -  "Uses descriptive activity names to name the activities in the data set"
#added an "activity" column (as a factor) with levels= and labels= argument set to columns in "labels" dataset
#converted "activity" from factor to character class

labels$activity
Xmeanstd$activity <- Xmeanstd$activityID
Xmeanstd$activity <- factor(Xmeanstd$activity,levels=1:6, labels = labels$activity)
#Xmeanstd$activity <-   as.character(Xmeanstd$activity)

#OBJECTIVE 4 - "Appropriately labels the data set with descriptive variable names."
#Feature names added to the dataset previously before objective 1 was completed (see above)
#removed "-" and "()" characters from names
#Changed "mean" and "std" to sentence case (capitalized)

names <- gsub("-","",names(Xmeanstd))
names <- sub("[(]","",names)
names <- sub("[)]","",names)
names <- sub("mean","Mean",names)
names <- sub("std","Std",names)
head(names,15)

names(Xmeanstd) <- names

#OBJECTIVE 5 - "From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject."
#Data.frame was converted to a "tibble dataframe" using the dply package
#unneeded columns (i.e. "set" and "activityID") were removed
#dataframe was grouped first by subject then activity
#the mean was calculated across all columns
#the resulting summarized data was written to a .txt file

library(dplyr)

Xdata <- tbl_df(Xmeanstd)

Xdata_sum<-Xdata %>%
            select(-set,-activityID) %>%
            group_by(subject,activity) %>%
            summarise_all(mean) 

write.table(Xdata_sum,"UCI HAR Dataset/TidyData.txt", row.names=FALSE, sep=" ",col.names=TRUE)


TidyData <- read.table("UCI HAR Dataset/TidyData.txt",header=TRUE, stringsAsFactors = FALSE)


