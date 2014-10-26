library(plyr)

# Load training data
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
head(x_train,2)

# Load testing data
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

# Merges the training and the test sets to create one data set with subject and 
training_set <- cbind(cbind(x_train, subject_train), y_train)
testing_set <- cbind(cbind(x_test, subject_test),y_test)
data_set <- rbind(training_set, testing_set)

# Load the activity labels
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", colClasses = c("character"))
names(activity_labels) <- c("ActivityID","Activity")

# Load the feature file 
features <- read.table("UCI HAR Dataset/features.txt", colClasses = c("character"))

# Label the columns of the data set with names in features file and also label the subject, activityid column
data_labels <- rbind(rbind(features, c(length(x_train)+2, "Subject")),c(length(x_train)+1, "ActivityID"))
names(data_set) <- data_labels[,2]

# Extracts only the measurements on the mean and standard deviation for each measurement
mean_std_col <- grepl(".*mean.*|.*std.*|Subject|ActivityID", names(data_set))
mean_std_data <- data_set[,mean_std_col]

# Label activities in the data set using names from activity_labels file
mean_std_withlabels <- merge(mean_std_data, activity_labels, by = "ActivityID")
mean_std_noActID <- mean_std_withlabels[,names(mean_std_withlabels)!="ActivityID"]

# Label the data set with appropriate descriptive variable names
data_names <- names(mean_std_noActID)

data_names <- gsub("\\()","",data_names)
data_names <- gsub("-std","StdDev",data_names)
data_names <- gsub("-mean","Mean",data_names)
data_names <- gsub("^t","Time",data_names)
data_names <- gsub("^f","Freq",data_names)
data_names <- gsub("Gyro","Gyroscope",data_names)
data_names <- gsub("Acc","Accelerometer",data_names)
data_names <- gsub("Mag","Magnitude",data_names)

names(mean_std_noActID) <- data_names

# Calculate the mean of each variable for each activity and each subject
tidy_data <- ddply(mean_std_noActID,c("Subject","Activity"),numcolwise(mean))

# save the final data set to a txt file
write.table(tidy_data,file="./tidy_data.txt",sep="\t",row.name=FALSE)