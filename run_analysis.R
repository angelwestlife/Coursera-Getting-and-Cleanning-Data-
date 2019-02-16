setwd('C:/Users/meinv/Documents/R Projects/R Exercise/')


library(dplyr)
library(data.table)
# 1.Merges the training and the test sets to create one data set.
test_path<-"C:/Users/meinv/Documents/R Projects/R Exercise/UCI HAR Dataset/test/X_test.txt"
train_path<-"C:/Users/meinv/Documents/R Projects/R Exercise/UCI HAR Dataset/train/X_train.txt"


#####Read in labels####
activity_labels <- read.table("C:/Users/meinv/Documents/R Projects/R Exercise/UCI HAR Dataset/activity_labels.txt"
                              , header = FALSE, col.names = c("actnum", "actdesc"))
features <- read.table("C:/Users/meinv/Documents/R Projects/R Exercise/UCI HAR Dataset/features.txt"
                       , header = FALSE, col.names = c("num","desc"))


####Read in test(subject_test, X_test and y_test)  and training (subject_train, X_train and y_train) datasets####
test<-read.table(test_path,header=FALSE)
subject_test<-read.table("C:/Users/meinv/Documents/R Projects/R Exercise/UCI HAR Dataset/test/subject_test.txt",
                         header=FALSE, col.names = "subject")
y_test<-read.table("C:/Users/meinv/Documents/R Projects/R Exercise/UCI HAR Dataset/test/y_test.txt",
                                 header=FALSE, col.names = "actnum")



train<-read.table(train_path,header=FALSE)
subject_train<-read.table("C:/Users/meinv/Documents/R Projects/R Exercise/UCI HAR Dataset/train/subject_train.txt",
                          header=FALSE, col.names = "subject")
y_train<-subject_test<-read.table("C:/Users/meinv/Documents/R Projects/R Exercise/UCI HAR Dataset/train/y_train.txt",
                                 header=FALSE, col.names = "actnum")



###Assign column labels to the test/train data####
colnames(test)<-features$desc
colnames(train)<-features$desc

####Combine Test and Training data####
combined_x<-rbind(test,train)
combined_y<-rbind(y_test,y_train)
combined_sub<-rbind(subject_test,subject_train)





# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
check_name<-names(combined_x)
test<-grepl('[Mm]ean|[Ss][Tt][Dd]',check_name)
measure_mean_std<-combined_x[,test]
###Combine with suject and activity description
combined_all<-cbind(measure_mean_std,combined_y,combined_sub)


# 3. Uses descriptive activity names to name the activities in the data set
combined_all<-combined_all %>%
  left_join(activity_labels,by="actnum")


# 4. Appropriately labels the data set with descriptive variable names.
names(combined_all)
names(combined_all)<-gsub("^t", "Time", names(combined_all))
names(combined_all)<-gsub("^f", "Freq", names(combined_all))
names(combined_all)<-gsub("BodyBody", "Body", names(combined_all))
names(combined_all)<-gsub("tBody", "TimeBody", names(combined_all))
names(combined_all)<-gsub("\\(\\)", "", names(combined_all))


# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
# variable for each activity and each subject.

tidy_data<-combined_all %>%
  group_by(actnum,actdesc,subject) %>%
  summarise_all(mean)



# write the tidy data set to a file
write.csv(tidy_data, "tidy_data.csv", row.names=FALSE)
