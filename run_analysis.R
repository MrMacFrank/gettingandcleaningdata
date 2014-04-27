### Programming Assignment
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  
# 
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 
#   
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
# Here are the data for the project: 
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# You should create one R script called run_analysis.R that does the following. 
# 1 Merges the training and the test sets to create one data set.
# 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3 Uses descriptive activity names to name the activities in the data set
# 4 Appropriately labels the data set with descriptive activity names. 
# 5 Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
# Good luck!


#clean up existing workspace
rm(list=ls())

############################################################################################
# 1. Merge datasets
# load data first.

# download only, if necessary:
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "dataset.zip", method="curl")

# data files (.zip!) were extracted manually
data.test <- read.table("Dataset/test/X_test.txt", header=FALSE) # read TEST data
data.train<- read.table("Dataset/train/X_train.txt", header=FALSE)  # read TRAINING data

data.test.subject <- read.table("Dataset/test/subject_test.txt", header=FALSE, stringsAsFactors=FALSE) # read subjects in TEST
data.train.subject <- read.table("Dataset/train/subject_train.txt", header=FALSE, stringsAsFactors=FALSE) # read subjects in TRAINING

data.test.activity <- read.table("Dataset/test/y_test.txt", header=FALSE, stringsAsFactors=FALSE) # read activity data in TEST
data.train.activity <- read.table("Dataset/train/y_train.txt", header=FALSE, stringsAsFactors=FALSE) # read activity data in TRAINING

activitylabels <- read.table("Dataset/activity_labels.txt", header=FALSE, stringsAsFactors=FALSE) # read activity LABELS

features <- read.table("Dataset/features.txt", header=FALSE , stringsAsFactors=FALSE) # read name of VARIABLES

complete.test <- cbind(data.test.subject, data.test.activity, data.test) # create one dataset for TEST
complete.train <- cbind(data.train.subject, data.train.activity, data.train) # create one dataset for TRAINING

#head(complete.train)[1:15, 1:15]
data.all <- rbind(complete.test, complete.train) # rbind those datasets to one dataset (data.all)

#names(data.all)
#View(features)
namestring<-c("subjects", "activity", features$V2) # create namevector for variables (columns)
names(data.all)<-namestring # apply names to variables

# remove all other data frames
rm(list=c("namestring", "features", "data.test.subject", "data.train.subject", "data.test.activity","data.train.activity", "data.test", "activitylabels", "data.train", "complete.test", "complete.train"))


############################################################################################
# 2 Extract only the measurements on the mean and standard deviation for each measurement. 

# this would extract each and every variable with "mean" or "std" in its name:
# mean.std.data<-data.all[namestring[grep("mean|std", namestring, ignore.case=FALSE)]]

# As I'd like to exclude all variables that represent a "Freq" (frequency), I'll take a different approach
stringvec <- c("-mean()", "-std()") # "stringvec" indicates what strings have to be part of the variable names
usevar <- features$V2[grep(x=features$V2, pattern=paste(stringvec, collapse="|"), perl=FALSE,fixed=FALSE)]
View(usevar)

usevar <- usevar[-grep(usevar, pattern="Freq")] # exclude variables that are named e.g. "FreqMean"

data.onlyMeans <- data.all[,usevar] # use only Means (+SD) variables in the next data frame


############################################################################################
# 3 Uses descriptive activity names to name the activities in the data set

# ?as.factor
# View(activitylabels)
# data.all$activity<-factor(data.all$activity, labels=activitylabels$V2)
# View(data.all$activity)

subjects <- rbind(data.test.subject, data.train.subject) # adds subjects for training and test
activity <- rbind(data.test.activity, data.train.activity) # adds activities for training and test

data.full <- data.frame(cbind(subjects, activity, data.onlyMeans)) # data combined with subjects and activities
#head(data.full)[1:6]

data.full$V1.1 <- factor(data.full$V1.1, labels = activitylabels$V2) # declare factors and give meaningful labels
data.full$V1 <- factor(data.full$V1, labels = 1:30)

head(data.full)[1:6]


############################################################################################
# 4 Appropriately labels the data set with descriptive activity names. 
# I'm confused by the instruction. Therefore I'd suggest cleaning the variables' names as to not containing
# any upper case or special cases in order to have consistency in the names to improve readability

usevar <- gsub("\\( | \\) | \\-", "", usevar) # delete all (,), and -
usevar <- tolower(usevar)   # all lower case (see lecture week 4)

names(data.full)<-c("subject", "activity", usevar)

head(data.full)


############################################################################################
# 5 Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# install package "plyr" if not installed. else skip this part
install.packages("plyr")
library(plyr)

data.final <- ddply(data.full,.(subject, activity), colwise(mean)) # compute means for subjects and activities

#head(data.final)


save(data.final, list=ls(all=TRUE), file="data.final.RData") # save data.final as .RData file

