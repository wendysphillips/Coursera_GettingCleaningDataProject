#The goals of this assignment are as below.

#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABQAAAASCAYAAABb0P4QAAABCklEQVR42u3Tv2rCUBQGcCkFF11cXCooDoEo4tAgaZdMhTyIu2PBQRydBBfXTtIX8IF8iOTcvzk9J8S0EBsSHerQCx/c4d4f383NbTT+x3L3ie/bQ2nmq32aMAybpdhi84Hr4wljiAsBwQEUUqBUMl3HaCnICy6CBIFkTKIiTGt9HciNAL5bSaVQGY3amPogZKAQhHEzrQjSaKxBa209kFsBt8qa8RENtbKUhDBMknpg1fwtqPi7naN0GgHiNjCf8+1S+LKqgA+u67aGY6/3K5g1jeOoAD6N/E4QBI+55jiv7b7rTQfj2dtP0Bibz/nX4URREeR9lO7FhsOp/3J+p1UymDw7hYZ3P74A6xEjKDEtv3EAAAAASUVORK5CYII=labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#read the features file
features <- read.csv("~/Downloads/UCI HAR Dataset/features.txt", sep = "", header = FALSE)

#read the testing and training data
test <- read.csv("~/Downloads/UCI HAR Dataset/test/X_test.txt", sep = "", header = FALSE)
train <- read.csv("~/Downloads/UCI HAR Dataset/train/X_train.txt", sep = "", header = FALSE)

#read activity labels file
activitykey <- read.csv("~/Downloads/UCI HAR Dataset/activity_labels.txt", sep = "", header = FALSE)

#read test and training labels files
testlab <- read.csv("~/Downloads/UCI HAR Dataset/test/y_test.txt", sep = "", header = FALSE)
trainlab <- read.csv("~/Downloads/UCI HAR Dataset/train/y_train.txt", sep = "", header = FALSE)

#read subject labels file
subtrainlab <- read.csv("~/Downloads/UCI HAR Dataset/train/subject_train.txt", sep = "", header = FALSE)
subtestlab <- read.csv("~/Downloads/UCI HAR Dataset/test/subject_test.txt", sep = "", header = FALSE)


#merge testing and training data sets
merged <- rbind(test,train)

#Add descriptive variable names to the column heads of the data set. The descriptive names are taken from the features.txt file. Extended description of the variables are given in the CodeBook.md file.
feats <- as.vector(features[,2])
names(merged) <- feats

#Create a list for exact matches of std() and mean()
searchvec <- c("\\<std()\\>", "\\<mean()\\>")

#Select only columns with means and standard deviations
mergedmin <- merged[ grep(paste(searchvec, collapse = "|"), names(merged), ignore.case = TRUE) ]

#make a single vector of activity codes for test and training data
actcodes <-as.vector(rbind(testlab, trainlab))

#make a single vector of subject codes for test and training data
subcodes <-as.vector(rbind(subtestlab, subtrainlab))

#add columns with activity and subject codes to data sets
wact <- cbind(actcodes, subcodes, mergedmin)


#add names to activity and subject columns of data frame
names(wact)[1] <- "activity"
names(wact)[2] <- "subject"

#add names to the two columns in the activity description file
names(activitykey) <- c("activity", "movement")

#Use the activity file to add a column with descriptive activity names by merging the activity codes file with the near-final data set. 
wact$activity <- activitykey[,2][match(wact$activity, activitykey[,1])]

#create a final tidy data set with the average of each variable for each activity and each subject
tidyset <- group_by(wact, subject, activity) %>%
      summarise_each(funs(mean))

#write the final data set to a csv file
write.csv(tidyset, "tidyset.csv")