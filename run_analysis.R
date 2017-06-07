#Assigment 1/Week 4/Course 4

#CHANGE DIRECTORY
setwd("C:/Users/rafaela.becerra/Desktop/Week 4/Deber/UCI HAR Dataset")

#1.Merges the training and the test sets to create one data set.
# Read general data
features<-read.table("features.txt")
activity_labels<-read.table("activity_labels.txt")
colnames(activity_labels)[[1]]<-"activity_ID"

#Read test data
subject_test<-read.table("subject_test.txt")
X_test<-read.table("X_test.txt")
y_test<-read.table("y_test.txt")
#Change names and assign features to X_test
colnames(subject_test)<-"subject_ID"
colnames(y_test)<-"activity_ID"
colnames(X_test)<-features[,2]
#join
testdata<-cbind(subject_test, y_test, X_test)

#Read train data
subject_train<-read.table("subject_train.txt")
X_train<-read.table("X_train.txt")
y_train<-read.table("y_train.txt")
#Change names and assign features to X_train
colnames(subject_train)<-"subject_ID"
colnames(y_train)<-"activity_ID"
colnames(X_train)<-features[,2]
#join
traindata<-cbind(subject_train, y_train, X_train)

#join train y test data
data<-rbind(testdata, traindata)

#2.Extracts only the measurements on the mean and standard deviation for each 
#measurement.

datamean_sd<-data[,grep("mean|std|subject_ID|activity_ID", colnames(data))]

#3.Uses descriptive activity names to name the activities in the data set
library(dplyr)

data<-left_join(datamean_sd, activity_labels, by="activity_ID")
data<-select(data, 1, 82, 3:81)
colnames(data)[[2]]<-"activity"

#4.Appropriately labels the data set with descriptive variable names.
names(data) <- gsub("\\(|\\)", "", names(data), perl  = TRUE)
names(data) <- gsub("Mag", " Magnitude", names(data), perl  = TRUE)
names(data) <- gsub("Body", " Body ", names(data), perl  = TRUE)
names(data) <- gsub("Gravity", " Gravity ", names(data), perl  = TRUE)
names(data) <- gsub("Gyro", " Gyroscope ", names(data), perl  = TRUE)
names(data) <- gsub("Acc", "Accelerometer", names(data), perl  = TRUE)
names(data) <- gsub("Jerk", " Jerk", names(data), perl  = TRUE)
names(data) <- gsub(" -", "-", names(data), perl  = TRUE)
names(data) <- gsub("Body", "Body", names(data), perl  = TRUE)
names(data) <- gsub("Body Body", "Body", names(data), perl  = TRUE)
names(data) <- gsub("meanFreq", "Mean Frequency", names(data), perl  = TRUE)
names(data) <- gsub("mean", "Mean", names(data), perl  = TRUE)
names(data) <- gsub("std", "Std", names(data), perl  = TRUE)
names(data) <- gsub("^f", "Frequency", names(data), perl  = TRUE)
names(data) <- gsub("^t", "Time", names(data), perl  = TRUE)

#5.From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.

data2<-data%>%group_by(subject_ID,activity)%>%summarise_each(funs(mean))
names(data2) <- gsub("^T", "Average T", names(data2), perl  = TRUE)
names(data2) <- gsub("^F", "Average F", names(data2), perl  = TRUE)

write.table(data2, "data.txt", row.names=FALSE)
