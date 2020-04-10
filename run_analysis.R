filename <- "CourseProject.zip"
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, filename, method="curl")
unzip(filename) 
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#1.Merges the training and the test sets to create one data set.
test<-cbind(x_test,y_test,subject_test)
train<-cbind(x_train,y_train,subject_train)
mergedData<-rbind(test,train)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
mergedData1 <- mergedData[ , names(mergedData) %in% c("code","subject") | 
                         grepl("\\bmean\\b", names( mergedData ) )|grepl("\\bstd\\b", names( mergedData ) ) ]

#3.Uses descriptive activity names to name the activities in the data set
mergedData2<-merge(mergedData1,activities,by='code')
cols.dont.want <- "code"
mergedData2 <- mergedData2[, ! names(mergedData2) %in% cols.dont.want, drop = F]

#4.Appropriately labels the data set with descriptive variable names.
features2<-features %>% filter(grepl ("\\bmean\\b|\\bstd\\b",functions))
colnames(mergedData2)[1:66]<-as.character(features2$functions)
mergedData2<-cbind(mergedData2[67:68],mergedData2[1:66])

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
TidyData<-mergedData2 %>% group_by(subject,activity) %>% summarise_all(mean)
write.table(TidyData, "TidyData", row.name=FALSE)
