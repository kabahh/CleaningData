## Reading in the datasets from working directory

x.test <- read.table("X_test.txt", sep="")
x.train <- read.table("X_train.txt", sep="")
y.test <- read.table("y_test.txt", sep="")
y.train <- read.table("y_train.txt", sep="")
sub.test <- read.table("subject_test.txt", sep="")
sub.train <- read.table("subject_train.txt", sep="")
features <- read.table("features.txt", sep="")
activities <- read.table("activity_labels.txt", sep="")

## Combining data sets into one data

x.data <- rbind(x.train, x.test)
y.data <- rbind(y.train, y.test)
sub.data <-rbind(sub.train, sub.test)
all.data <- cbind(x.data, sub.data, y.data)

## Adding features to data
names(all.data)<- features[,2]
colnames(all.data)[562] <- "subject"
colnames(all.data)[563] <- "activities"

## Adding labels to activities variable
all.data[,563] <- factor(all.data[,563], levels=c(1,2,3,4,5,6), labels=activities[,2])

## Retrieving only variables with either mean or std in their column name
test1<- grepl(".*mean", colnames(all.data), ignore.case=TRUE) | grepl(".*std", colnames(all.data), ignore.case=TRUE)

## Pulling the selected column and including the subject id and activity columns
data2 <- c(which(test1), 562, 563)
all.data2 <- all.data[,data2]

## Creating a factor variable out of the subject Ids

all.data2[,87] <- factor(all.data2[,87], levels=c(1:30))

## Creating the final aggregate dataset and calculating the grandmean for all variables over subject and activity
final.data <- aggregate(. ~ subject+activities,all.data2, mean)

## Renaming columns and activities to more human-understandable names
test2 <- names(final.data)
names1 <- gsub("tBodyAcc","timeBodyAcceleration",test2)
names1 <- sub("tGravityAcc","timeGravityAcceleration",names1)
names1 <- sub("tBodyGyro","timeBodyGyroscope",names1)
names1 <- sub("fBodyAcc","frequencyBodyAcceleration",names1)
names1 <- sub("fBodyGyro","frequencyBodyGyroscope",names1)
names1 <- sub("fBodyBodyAcc","frequencyBodyAcceleration",names1)
names1 <- sub("fBodyBodyGyro","frequencyBodyGyroscope",names1)
names1 <- sub("Mag","Magnitude",names1)
names1 <- sub("-mean\\()-","Mean",names1)
names1 <- sub("-std\\()-","Std",names1)
names1 <- sub("-mean\\()","Mean",names1)
names1 <- sub("-std\\()","Std",names1)
names1 <- sub("-meanFreq\\()-","WieghtedFreqMean",names1)
names1 <- sub("-meanFreq\\()","WieghtedFreqMean",names1)
names1 <- gsub("angle","angleBetween",names1)
names1 <- gsub("\\(","",names1)
names1 <- gsub("\\)","",names1)
names1 <- gsub(",","and",names1)
names1 <- tolower(names1)
names(final.data)[2] <- "activity"

final.data$activity <- tolower(gsub("_","",final.data$activity))

## Writes the final dataset into an independent data file
write.table(final.data, "tidydata.txt", sep="\t")

