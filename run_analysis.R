setwd("~/R/Coursera Getting and Cleaning Data Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")
library(dplyr)
#importing train set
X_train <- read.delim("train/X_train.txt",header=FALSE, sep="") 
#importing test set
X_test <- read.delim("test/X_test.txt", header=FALSE, sep="") 

#binding training and test sets, point 1
X_total <- rbind(X_train, X_test) 

#list features
features <- read.delim("features.txt", header=FALSE, sep="") 
#converting into vector
f <- features[,2] 

#labeling columns with descriptive variable names, point 4
X_total <- setNames(X_total, f) 

#quite some duplicated column names, none of them are mean or standard deviation variables
dup_names <- which(duplicated(names(X_total))) 
#removing the columns with duplicated names
X_total <- X_total[,-dup_names] 

#selecting only columns of mean and standard deviation variables, point 2
X_total <- select(X_total, matches("mean|std")) 

#train labels
y_train <- read.delim("train/y_train.txt", header=FALSE, sep="") 
#test labels
y_test <- read.delim("test/y_test.txt", header=FALSE, sep="") 
#binding train and test labels
y_total <- rbind(y_train, y_test) 
#linking labels to their activity name
y_total <- mutate(y_total, V1 = replace(V1, V1 == "1", "walking")) %>%
  mutate(V1 = replace(V1, V1 == "2", "walking_upstairs")) %>%
  mutate(V1 = replace(V1, V1 == "3", "walking_downstairs")) %>%
  mutate(V1 = replace(V1, V1 == "4", "sitting")) %>%
  mutate(V1 = replace(V1, V1 == "5", "standing")) %>%
  mutate(V1 = replace(V1, V1 == "6", "laying")) 
#renaming column
names(y_total)[names(y_total)=="V1"] <- "Activity" 

#adding descriptive activities to the dataset, point 3
X_total <- cbind(y_total, X_total) 

#splitting dataset into the activities
Activity <- split(X_total, X_total$Activity) %>% 
  sapply(function(x){ 
    colMeans(x[2:87]) #taking the mean for each activity
  }) %>%
  t() #flipping the rows and columns
#adding column in front with the activities
Activity <- data.frame(Activity_Subject = c("laying", "sitting", "standing", "walking", "walking_downstairs", "walking_ustairs"), Activity)

#importing train subject labels
train_subject <- read.delim("train/subject_train.txt",header=FALSE, sep="") 
#importing test subect labels
test_subject <- read.delim("test/subject_test.txt", header=FALSE, sep="") 
#binding subject lists
subjects_list <- rbind(train_subject, test_subject) 
#adding subject lists in front
X_total2 <- cbind(subjects_list, X_total) 

#splitting dataset into subjects
Subjects <- split(X_total2, X_total2$V1) %>% 
  sapply(function(x){
    colMeans(x[3:88]) #taking the mean for each subject
  }) %>%
  t() #fliping the rows and columns

#adding column in front with the subjects
Subjects <- data.frame(Activity_Subject = c("subject_1", "subject_2", "subject_3", "subject_4", 
                     "subject_5", "subject_6", "subject_7", "subject_8", 
                     "subject_9", "subject_10", "subject_11", "subject_12", 
                     "subject_13", "subject_14", "subject_15", "subject_16", 
                     "subject_17", "subject_18", "subject_19", "subject_20", 
                     "subject_21", "subject_22", "subject_23", "subject_24", 
                     "subject_25", "subject_26", "subject_27", "subject_28", 
                     "subject_29", "subject_30"), Subjects)

#dataset with the average of each variable for each activity and each subject, point 5
Mean_total <- rbind(Activity, Subjects) 
