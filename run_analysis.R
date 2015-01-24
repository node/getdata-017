##check required packages
# nothing to do 

## download data if needed (or download data by yourself) 
#fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#download.file(fileUrl,destfile="./rawdata.zip",method="auto")
#unzip(zipfile="./rawdata.zip",exdir="./")


## load data 
train.x<-read.table("./UCI HAR Dataset/train/X_train.txt")
train.y<-read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")

test.x<-read.table("./UCI HAR Dataset/test/X_test.txt")
test.y<-read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")

activity_lables<-read.table("./UCI HAR Dataset/activity_labels.txt")
features<-read.table("./UCI HAR Dataset/features.txt")


# 1. Merges the training and the test sets to create one data set.
all.x<-rbind(train.x,test.x)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
colnames(all.x) <- c(as.character(features[,2]))

Mean<-grep("mean()",colnames(all.x),fixed=TRUE)
SD<-grep("std()",colnames(all.x),fixed=TRUE)
MeanSD<-all.x[,c(Mean,SD)]

# 3. Uses descriptive activity names to name the activities in the data set
all.y<-rbind(train.y,test.y)
all.activity<-cbind(all.y,MeanSD)
colnames(all.activity)[1] <- "Activity"

# 4. Appropriately labels the data set with descriptive variable names. 
activity_lables[,2]<-as.character(activity_lables[,2])

for(i in 1:length(all.activity[,1])){
  all.activity[i,1]<-activity_lables[all.activity[i,1],2]
}

# 5. From the data set in step 4, creates a second, 
#    independent tidy data set with the average of each variable for each activity and each subject.

subject_all<-rbind(subject_train,subject_test)

all<-cbind(subject_all,all.activity)
colnames(all)[1] <- "Subject"
tidy <- aggregate( all[,3] ~ Subject+Activity, data = all, FUN= "mean" )

for(i in 4:ncol(all)){
  tidy[,i] <- aggregate( all[,i] ~ Subject+Activity, data = all, FUN= "mean" )[,3]
}

colnames(tidy)[3:ncol(tidy)] <- colnames(MeanSD)

write.table(tidy, file = "final.txt",row.name=FALSE)

#View(tidy)
#t<-read.table("./final.txt")
#View(t)
